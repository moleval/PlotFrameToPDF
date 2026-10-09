;;; -*- coding: windows-1251 -*-
;;; PlotFrameToPDF.lsp
;;;
;;; Экспорт области, выделенной рамкой, в PDF (DWG To PDF.pc3).
;;;
;;; v3.12: возврат к печати через -ПЕЧАТЬ как основному механизму
;;;        (окно печати через ActiveX в ряде конфигураций не применяется).
;;;        Ключевое исправление: лента ответов -ПЕЧАТЬ собирается ПО
;;;        ВКЛАДКЕ (Модель или Лист — разные наборы вопросов; именно
;;;        отсюда были файлы "_N.pdf" и вопрос «Продолжить построение?»).
;;;        Убраны «холостые Enter» из сброса состояния: они могли
;;;        запускать повтор команды внутри выполняющейся команды
;;;        (ошибка "неверный тип аргумента: stringp T" на второй печати).
;;;        Имя CTB передаётся как есть (без проверки findfile — папка
;;;        стилей печати может не входить в пути поиска).
;;;        Любая ошибка печати перехватывается и печатается с контекстом.
;;;        ActiveX-механизм сохранён как опция: *pfp-engine* "activex".
;;;
;;; Команды: ЭКСВПДФ / EXPTPDF / ОЧИСТПДФ / ПФПТАБЛ / ПФПСТАТ / ПФПМЕДИА
;;;          ПФПЦВЕТ / ПФПЧБ / ПФППАПКА / ПФПТЕМП

(vl-load-com)

(setq *pfp-ver* "3.12")

(setq *pfp-open-mode* "rundll")
(setq *pfp-open-delay* 300)

(setq *pfp-auto-format*      T)
(setq *pfp-consider-area*    T)
(setq *pfp-size-factor*      2.0)
(setq *pfp-unit-scale*       1.0)
(setq *pfp-max-format*       nil)
(setq *pfp-rotate-invert*    nil)
(setq *pfp-force-orient*     nil)
(setq *pfp-fixed-format*     "ISO_full_bleed_A1_(841.00_x_594.00_MM)")

(setq *pfp-block-size*       50)
(setq *pfp-subdir*           "PlotFramePDF")
(setq *pfp-color-mode*       nil)
(setq *pfp-ctb-mono*         "monochrome.ctb")
(setq *pfp-ctb-color*        "acad.ctb")
(setq *pfp-output-dir*       nil)

;; "command" — печать через -ПЕЧАТЬ (рекомендуется);
;; "activex" — через Plot.PlotToFile (в ряде конфигураций не применяет
;;             окно печати и стиль — использовать осознанно).
(setq *pfp-engine* "command")
;; nil — после печати вернуть настройки листа, как было
;; (файл не «запоминает» DWG To PDF); T — оставить как есть.
(setq *pfp-keep-page-setup* nil)

;; ---------------------------------------------------------------------------
;; Таблица форматов
;; (короткое  W_L  H_L  "портрет"  "альбом"  верх_объектов)
;; ---------------------------------------------------------------------------
(setq *pfp-formats*
  '(("A3"   420  297
     "ISO_full_bleed_A3_(297.00_x_420.00_MM)"
     "ISO_full_bleed_A3_(420.00_x_297.00_MM)"
     25)
    ("A2"   594  420
     "ISO_full_bleed_A2_(420.00_x_594.00_MM)"
     "ISO_full_bleed_A2_(594.00_x_420.00_MM)"
     50)
    ("A1"   841  594
     "ISO_full_bleed_A1_(594.00_x_841.00_MM)"
     "ISO_full_bleed_A1_(841.00_x_594.00_MM)"
     100)
    ("A0"  1189  841
     "ISO_full_bleed_A0_(841.00_x_1189.00_MM)"
     "ISO_full_bleed_A0_(841.00_x_1189.00_MM)"
     200)
    ("2A0" 1682 1189
     "ISO_full_bleed_2A0_(1189.00_x_1682.00_MM)"
     "ISO_full_bleed_2A0_(1682.00_x_1189.00_MM)"
     400)
    ("4A0" 2378 1682
     "ISO_full_bleed_4A0_(1682.00_x_2378.00_MM)"
     "ISO_full_bleed_4A0_(2378.00_x_1682.00_MM)"
     999999)))

;; ---------------------------------------------------------------------------
;; Утилиты
;; ---------------------------------------------------------------------------

(defun pfp-get (obj prop)
  (vl-catch-all-apply 'vlax-get-property (list obj prop)))

(defun pfp-safe-put (obj prop val)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 'vlax-put-property (list obj prop val)))))

(defun pfp-default-dir ()
  (strcat (getenv "TEMP") "\\" *pfp-subdir*))

(defun pfp-output-dir ( / d )
  (setq d (if *pfp-output-dir* *pfp-output-dir* (pfp-default-dir)))
  (if (not (vl-file-directory-p d)) (vl-mkdir d))
  d)

(defun pfp-taken-p (dir n)
  (or (findfile (strcat dir "\\" (itoa n) ".pdf"))
      (findfile (strcat dir "\\" (itoa n)))))

(defun pfp-block-free-p (dir start len / i ok)
  (setq i 0 ok T)
  (while (and ok (< i len))
    (if (pfp-taken-p dir (+ start i)) (setq ok nil))
    (setq i (1+ i)))
  ok)

(defun pfp-next-number (dir / n)
  (setq n 1)
  (while (not (pfp-block-free-p dir n *pfp-block-size*))
    (setq n (1+ n)))
  n)

(defun pfp-sleep (ms / old-cmdecho)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.delay" ms)
  (setvar "CMDECHO" old-cmdecho))

;; Сброс зависшего состояния командной строки.
;; Только Esc, только пока CMDACTIVE > 0. Пустые Enter НЕ отправляются:
;; на пустой командной строке Enter повторяет последнюю команду,
;; что запускало вложенный запуск самой себя.
(defun pfp-clear-stuck ( / doc i)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (setq i 0)
  (while (and (< i 5) (> (getvar "CMDACTIVE") 0))
    (vl-catch-all-apply 'vla-SendCommand (list doc (chr 27)))
    (setq i (1+ i)))
  (princ))

;; Восстановление системных переменных после печати.
(defun pfp-restore-sys (has-trans old-trans old-bg old-paper)
  (if has-trans
    (vl-catch-all-apply 'setvar
      (list "PLOTTRANSPARENCYOVERRIDE" old-trans)))
  (setvar "BACKGROUNDPLOT" old-bg)
  (vl-catch-all-apply 'setvar (list "PAPERUPDATE" old-paper))
  (princ))

;; ---------------------------------------------------------------------------
;; Выбор формата
;; ---------------------------------------------------------------------------

(defun pfp-format-idx (fmt / i lst found)
  (setq i 0 lst *pfp-formats* found nil)
  (while (and lst (not found))
    (if (equal (nth 0 (car lst)) fmt) (setq found i))
    (setq i (1+ i) lst (cdr lst)))
  found)

(defun pfp-format-by-count (n-obj / pair)
  (setq pair *pfp-formats*)
  (while (and (cdr pair) (>= n-obj (nth 5 (car pair))))
    (setq pair (cdr pair)))
  (car pair))

(defun pfp-format-by-area (w-mm h-mm / pair res sz fw fh)
  (setq pair *pfp-formats* res nil)
  (while (and pair (not res))
    (setq sz (car pair)
          fw (* (nth 1 sz) *pfp-size-factor*)
          fh (* (nth 2 sz) *pfp-size-factor*))
    (if (or (and (>= fw w-mm) (>= fh h-mm))
            (and (>= fh w-mm) (>= fw h-mm)))
      (setq res sz))
    (setq pair (cdr pair)))
  (if res res (car (reverse *pfp-formats*))))

(defun pfp-want-landscape (w h)
  (cond
    ((equal *pfp-force-orient* "portrait")  nil)
    ((equal *pfp-force-orient* "landscape") T)
    (t (>= w h))))

(defun pfp-pick-format (n-obj w-mm h-mm / by-cnt by-area idx-cnt idx-area idx cap-idx rec want-land media)
  (if (not *pfp-auto-format*)
    (progn
      (setq want-land (pfp-want-landscape w-mm h-mm))
      (list "fix"
            *pfp-fixed-format*
            (if *pfp-rotate-invert*
              (if want-land "_P" "_L")
              (if want-land "_L" "_P"))))
    (progn
      (setq by-cnt  (pfp-format-by-count n-obj)
            idx-cnt (pfp-format-idx (nth 0 by-cnt))
            idx-area idx-cnt)
      (if *pfp-consider-area*
        (progn
          (setq by-area (pfp-format-by-area w-mm h-mm))
          (if by-area (setq idx-area (pfp-format-idx (nth 0 by-area))))))
      (setq idx (max idx-cnt idx-area))
      (if *pfp-max-format*
        (progn
          (setq cap-idx (pfp-format-idx *pfp-max-format*))
          (if (and cap-idx (> idx cap-idx)) (setq idx cap-idx))))
      (setq rec (nth idx *pfp-formats*)
            want-land (pfp-want-landscape w-mm h-mm)
            media (if want-land (nth 4 rec) (nth 3 rec)))
      (list (nth 0 rec)
            media
            (if *pfp-rotate-invert*
              (if want-land "_P" "_L")
              (if want-land "_L" "_P"))))))

;; ---------------------------------------------------------------------------
;; Media name: естественная ориентация, проверка по устройству
;; ---------------------------------------------------------------------------

;; Естественная ориентация из canonical-имени: разбирает "(W_x_H)".
;; W >= H -> "L", иначе "P".
(defun pfp-media-natural-orient (media-name / i pw ph)
  (setq i (vl-string-search "(" media-name))
  (if (null i)
    "L"
    (progn
      (setq pw (atof (substr media-name (+ i 2)))
            i  (vl-string-search "_x_" media-name))
      (if (null i)
        "L"
        (progn
          (setq ph (atof (substr media-name (+ i 4))))
          (if (>= pw ph) "L" "P"))))))

;; Rotation constant so that -PLOT semantics are preserved:
;; rot-str "_L"/"_P" relative to the media's natural orientation.
(defun pfp-rotation (media-name rot-str)
  (if (equal (substr rot-str 2 1) (pfp-media-natural-orient media-name))
    acPlotRotation0
    acPlotRotation90))

(defun pfp-ensure-consts ()
  (if (not (boundp 'acWindow))         (setq acWindow 4))
  (if (not (boundp 'acScaleToFit))     (setq acScaleToFit -1))
  (if (not (boundp 'acPlotRotation0))  (setq acPlotRotation0 0))
  (if (not (boundp 'acPlotRotation90)) (setq acPlotRotation90 1))
  (princ))

;; Список canonical media name активного устройства данного листа.
(defun pfp-layout-media-list (layout / lst v)
  (setq lst (vl-catch-all-apply 'vlax-invoke-method
                                (list layout 'GetCanonicalMediaNames)))
  (if (or (null lst) (vl-catch-all-error-p lst))
    nil
    (progn
      (setq v (vl-catch-all-apply 'vlax-variant-value (list lst)))
      (if (vl-catch-all-error-p v)
        nil
        (progn
          (setq lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
          (if (vl-catch-all-error-p lst) nil lst))))))

;; Замена формата на реально существующий в устройстве:
;; 1) точное имя; 2) любой лист того же формата ("_A1_"), желательно
;;    нужной ориентации; 3) любой full bleed A0 той же ориентации;
;; 4) первый доступный.
(defun pfp-pick-valid-media (media-name short-name rot-str lst
                             / nm want found found2)
  (setq want (substr rot-str 2 1))
  (cond
    ((member media-name lst) media-name)
    ((progn (setq found nil found2 nil)
            (foreach nm lst
              (if (vl-string-search
                    (strcat "_" (strcase short-name) "_")
                    (strcase nm))
                (if (and (not found)
                         (equal (pfp-media-natural-orient nm) want))
                  (setq found nm)
                  (if (not found2) (setq found2 nm)))))
            (or found found2)))
    ((progn (setq found nil found2 nil)
            (foreach nm lst
              (if (vl-string-search "FULL_BLEED_A0_" (strcase nm))
                (if (and (not found)
                         (equal (pfp-media-natural-orient nm) want))
                  (setq found nm)
                  (if (not found2) (setq found2 nm)))))
            (or found found2)))
    (lst (car lst))
    (t media-name)))

;; ---------------------------------------------------------------------------
;; Основной механизм: ActiveX (без командной строки, без вопросов)
;; ---------------------------------------------------------------------------

(defun pfp-layout-state (layout / props val res)
  (setq props '(ConfigName CanonicalMediaName StyleSheet PlotType PlotRotation
                UseStandardScale StandardScale CenterPlot PlotWithPlotStyles
                PlotHidden))
  (foreach p props
    (setq val (pfp-get layout p))
    (if (not (vl-catch-all-error-p val))
      (setq res (cons (cons p val) res))))
  (reverse res))

(defun pfp-restore-layout (layout state / pair)
  (foreach pair state
    (pfp-safe-put layout (car pair) (cdr pair)))
  (princ))

(defun pfp-plot-by-activex (media-name short-name rot-str ctb-name llw urw
                            fname-pdf
                            / acad doc layout plot ok res old-quiet saved
                              lst valid)

  (pfp-ensure-consts)
  (setq ok nil)
  (setq acad (vlax-get-acad-object))
  (setq doc (pfp-get acad 'ActiveDocument))
  (cond
    ((or (null doc) (vl-catch-all-error-p doc))
     (princ "\nПФП: недоступен ActiveDocument."))
    (t
      (setq layout (pfp-get doc 'ActiveLayout))
      (if (or (null layout) (vl-catch-all-error-p layout))
        (princ "\nПФП: недоступен активный лист.")
        (progn
          (setq plot (pfp-get doc 'Plot))
          (if (or (null plot) (vl-catch-all-error-p plot))
            (princ "\nПФП: недоступен объект Plot.")
            (progn
              (vla-StartUndoMark doc)
              ;; Подавить диалоги печати («размер бумаги не найден» и др.)
              (setq old-quiet (pfp-get plot 'QuietErrorMode))
              (vl-catch-all-apply 'vla-put-QuietErrorMode
                                  (list plot :vlax-true))
              ;; Запомнить настройки листа, чтобы вернуть их после печати
              (if (not *pfp-keep-page-setup*)
                (setq saved (pfp-layout-state layout)))

              ;; 1. Устройство
              (if (pfp-safe-put layout 'ConfigName "DWG To PDF.pc3")
                (progn
                  ;; 2. Формат: проверка по фактическому списку PC3
                  (setq lst (pfp-layout-media-list layout))
                  (if lst
                    (progn
                      (setq valid (pfp-pick-valid-media media-name
                                                        short-name rot-str
                                                        lst))
                      (if (not (equal valid media-name))
                        (princ (strcat "\nПФП: формат \"" media-name
                                       "\" отсутствует в DWG To PDF.pc3 —"
                                       " использован \"" valid "\".")))
                      (setq media-name valid)))
                  ;; 3. Бумага и ориентация
                  (pfp-safe-put layout 'CanonicalMediaName media-name)
                  (pfp-safe-put layout 'PlotRotation
                                (pfp-rotation media-name rot-str))
                  ;; 4. Область печати — рамка
                  (pfp-safe-put layout 'PlotType acWindow)
                  (setq res (vl-catch-all-apply 'vla-SetWindowToPlot
                    (list layout
                          (vlax-3d-point (car llw) (cadr llw) 0.0)
                          (vlax-3d-point (car urw) (cadr urw) 0.0))))
                  (if (vl-catch-all-error-p res)
                    (princ "\nПФП: окно печати не задано (SetWindowToPlot)."))
                  ;; 5. Вписать, центрировать
                  (pfp-safe-put layout 'UseStandardScale :vlax-true)
                  (pfp-safe-put layout 'StandardScale acScaleToFit)
                  (pfp-safe-put layout 'CenterPlot :vlax-true)
                  ;; 6. Стиль печати (без findfile: папка стилей может
                  ;;    не входить в пути поиска — AutoCAD сам найдёт CTB)
                  (if (pfp-safe-put layout 'StyleSheet ctb-name)
                    (pfp-safe-put layout 'PlotWithPlotStyles :vlax-true)
                    (progn
                      (pfp-safe-put layout 'PlotWithPlotStyles :vlax-false)
                      (princ (strcat "\nПФП: стиль \"" ctb-name
                                     "\" не применён — печать без стилей."))))
                  (pfp-safe-put layout 'PlotHidden :vlax-false)
                  ;; 7. Печать в PDF
                  (setq res (vl-catch-all-apply 'vla-PlotToFile
                                (list plot fname-pdf)))
                  (if (and (not (vl-catch-all-error-p res))
                           (equal res :vlax-true))
                    (setq ok T)
                    (princ "\nПФП: PlotToFile вернул отказ.")))
                (princ "\nПФП: устройство DWG To PDF.pc3 не найдено."))

              ;; Вернуть настройки листа как было
              (if saved (pfp-restore-layout layout saved))
              (if (vl-catch-all-error-p old-quiet)
                (vl-catch-all-apply 'vla-put-QuietErrorMode
                                    (list plot :vlax-false))
                (vl-catch-all-apply 'vla-put-QuietErrorMode
                                    (list plot old-quiet)))
              (vla-EndUndoMark doc)))))))
  ok)

;; ---------------------------------------------------------------------------
;; Основной механизм: -ПЕЧАТЬ
;; ---------------------------------------------------------------------------
;; ВНИМАНИЕ: команда -ПЕЧАТЬ задаёт РАЗНЫЙ набор вопросов для вкладок
;; Модель и Лист:
;;   Лист:  «масштабировать веса», «лист первым», «удалять скрытые»;
;;   Модель: этих вопросов нет, вместо них «Задать тонирование»
;;           (ответ — ключевое слово, а не Да/Нет).
;; Лента ответов собирается по TILEMODE. Именно расхождение ленты
;; давало PDF с именем "_N" и оставшийся вопрос «Продолжить построение?».

(defun pfp-plot-by-command (fname-pdf media-name rot-str ctb-name x1 y1 x2 y2
                            / old-cmdecho tape i)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq tape
    (append
      (list "_.-plot"
            "_Y"                     ; подробная настройка
            ""                       ; текущий лист
            "DWG To PDF.pc3"         ; устройство
            media-name               ; формат
            "_M"                     ; мм
            rot-str                  ; ориентация _L / _P
            "_N"                     ; не переворачивать
            "_W"                     ; область: Рамка
            (list x1 y1)             ; нижний левый угол
            (list x2 y2)             ; верхний правый угол
            "_F"                     ; вписать
            "_C")                    ; центрировать
      (if (= 1 (getvar "TILEMODE"))
        ;; --- вкладка Модель ---
        (list
          "_Y"                     ; учитывать стили печати
          ctb-name                 ; CTB-файл
          "_Y"                     ; учитывать веса линий
          "_N"                     ; не масштабировать веса
          "_As"                    ; тонирование: по отображению
          fname-pdf                ; имя файла
          "_N"                     ; не сохранять в параметры
          "_Y")                    ; печатать
        ;; --- вкладка Лист ---
        (list
          "_Y"                     ; учитывать стили печати
          ctb-name                 ; CTB-файл
          "_Y"                     ; учитывать веса линий
          "_N"                     ; не масштабировать веса
          "_N"                     ; не чертить пространство листа первым
          "_N"                     ; не удалять скрытые линии
          fname-pdf                ; имя файла
          "_N"                     ; не сохранять в параметры
          "_Y"))))                  ; печатать
  (apply 'command tape)
  (setvar "CMDECHO" old-cmdecho)
  ;; дочитать возможный «хвост» значениями по умолчанию (с ограничением)
  (setq i 0)
  (while (and (> (getvar "CMDACTIVE") 0) (< i 15))
    (command "")
    (setq i (1+ i)))
  t)

;; ---------------------------------------------------------------------------
;; Открытие PDF
;; ---------------------------------------------------------------------------

(defun pfp-open-rundll (path / args)
  (setq args (strcat "url.dll,FileProtocolHandler \"" path "\""))
  (startapp "rundll32.exe" args))

(defun pfp-open-shell (path / sh)
  (setq sh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
  (if (and sh (not (vl-catch-all-error-p sh)))
    (progn
      (vl-catch-all-apply
        'vlax-invoke-method
        (list sh 'Run (strcat "\"" path "\"") 1 :vlax-false))
      (vl-catch-all-apply 'vlax-release-object (list sh)))
    (startapp "explorer.exe" path)))

(defun pfp-open-explorer (path) (startapp "explorer.exe" path))

(defun pfp-open (path)
  (cond
    ((= *pfp-open-mode* "shell")    (pfp-open-shell    path))
    ((= *pfp-open-mode* "explorer") (pfp-open-explorer path))
    (t                              (pfp-open-rundll   path))))

;; ---------------------------------------------------------------------------
;; Диалог выбора папки
;; ---------------------------------------------------------------------------

(defun pfp-browse-folder ( / shell folder self path)
  (setq shell (vl-catch-all-apply 'vlax-create-object (list "Shell.Application")))
  (if (or (null shell) (vl-catch-all-error-p shell))
    nil
    (progn
      (setq folder
            (vl-catch-all-apply
              'vlax-invoke-method
              (list shell 'BrowseForFolder 0
                    "Выберите папку для экспорта PDF" 1)))
      (vl-catch-all-apply 'vlax-release-object (list shell))
      (if (or (null folder) (vl-catch-all-error-p folder))
        nil
        (progn
          (setq self (pfp-get folder 'Self)
                path (if self (pfp-get self 'Path) nil))
          (vl-catch-all-apply 'vlax-release-object (list folder))
          (if (or (null path) (vl-catch-all-error-p path)) nil path))))))

;; ---------------------------------------------------------------------------
;; Форматирование
;; ---------------------------------------------------------------------------

(defun pfp-pad-left (s width / n)
  (setq n (- width (strlen s)))
  (while (> n 0) (setq s (strcat " " s) n (1- n))) s)

(defun pfp-pad-right (s width / n)
  (setq n (- width (strlen s)))
  (while (> n 0) (setq s (strcat s " ") n (1- n))) s)

(defun pfp-mode-str () (if *pfp-color-mode* "цветная" "ч/б"))

;; ---------------------------------------------------------------------------
;; Справки
;; ---------------------------------------------------------------------------

(defun pfp-print-table ( / pair prev thr short line w h wstr eff)
  (princ "\nТаблица форматов:")
  (princ "\nКритерий выбора — МАКСИМУМ из двух: количество объектов ИЛИ габариты рамки.")
  (princ (strcat "\n(в критерии по габаритам размеры формата умножаются на "
                 (rtos *pfp-size-factor* 2 2) ")"))
  (princ
    (strcat "\n  " (pfp-pad-left "формат" 8)
            "  " (pfp-pad-left "размер (мм)" 16)
            "  " (pfp-pad-left "объектов" 12)
            "  " "рамка вписывается до"))
  (princ
    (strcat "\n  " (pfp-pad-left "------" 8)
            "  " (pfp-pad-left "-----------" 16)
            "  " (pfp-pad-left "----------" 12)
            "  " "--------------------"))
  (setq pair *pfp-formats* prev 0)
  (foreach rec pair
    (setq short (nth 0 rec) w (nth 1 rec) h (nth 2 rec) thr (nth 5 rec)
          wstr  (strcat (itoa w) " x " (itoa h) " мм")
          eff   (strcat (itoa (fix (* w *pfp-size-factor*))) " x "
                        (itoa (fix (* h *pfp-size-factor*))) " мм")
          line  (strcat (itoa prev) ".."
                        (if (>= thr 999999) "..." (itoa (1- thr)))))
    (princ (strcat "\n  " (pfp-pad-left short 8)
                   "  " (pfp-pad-left wstr 16)
                   "  " (pfp-pad-left line 12)
                   "  " "до " eff))
    (setq prev thr))
  (princ))

(defun pfp-row (left right)
  (princ (strcat "\n  " (pfp-pad-right left 36) "  " right)))

(defun pfp-print-vars ( / dir-str max-str area-str orient-str engine-str keep-str)
  (setq dir-str    (if *pfp-output-dir* *pfp-output-dir* "nil (= %TEMP%\\PlotFramePDF)")
        max-str    (if *pfp-max-format* *pfp-max-format* "nil (без ограничения)")
        area-str   (if *pfp-consider-area* "T (учитывать)" "nil (только объекты)")
        orient-str (if *pfp-force-orient* *pfp-force-orient* "nil (авто)")
        engine-str (if (= *pfp-engine* "command")
                       "command (-ПЕЧАТЬ, запасной)"
                       "activex (PlotToFile)")
        keep-str   (if *pfp-keep-page-setup*
                       "T (оставить)"
                       "nil (вернуть как было)"))
  (princ "\nПеременные модуля:")
  (pfp-row "переменная" "назначение / текущее значение")
  (pfp-row "--------------------" "--------------------------------------------------------------")
  (pfp-row "*pfp-color-mode*"       (strcat "nil = ч/б, T = цвет  ["
                                            (if *pfp-color-mode* "T" "nil") "]"))
  (pfp-row "*pfp-ctb-mono*"         (strcat "CTB для ч/б  [" *pfp-ctb-mono* "]"))
  (pfp-row "*pfp-ctb-color*"        (strcat "CTB для цвета  [" *pfp-ctb-color* "]"))
  (pfp-row "*pfp-output-dir*"       (strcat "папка экспорта  [" dir-str "]"))
  (pfp-row "*pfp-consider-area*"    (strcat "учёт габаритов  [" area-str "]"))
  (pfp-row "*pfp-size-factor*"      (strcat "множитель  [" (rtos *pfp-size-factor* 2 2) "]"))
  (pfp-row "*pfp-unit-scale*"       (strcat "мм в ед.  [" (rtos *pfp-unit-scale* 2 3) "]"))
  (pfp-row "*pfp-max-format*"       (strcat "макс. формат  [" max-str "]"))
  (pfp-row "*pfp-force-orient*"     (strcat "форс ориент.  [" orient-str "]"))
  (pfp-row "*pfp-open-delay*"       (strcat "задержка  [" (itoa *pfp-open-delay*) "]"))
  (pfp-row "*pfp-open-mode*"        (strcat "способ откр.  [" *pfp-open-mode* "]"))
  (pfp-row "*pfp-block-size*"       (strcat "блок номеров  [" (itoa *pfp-block-size*) "]"))
  (pfp-row "*pfp-subdir*"           (strcat "подпапка  [" *pfp-subdir* "]"))
  (pfp-row "*pfp-engine*"           (strcat "механизм печати  [" engine-str "]"))
  (pfp-row "*pfp-keep-page-setup*"  (strcat "настройки листа  [" keep-str "]"))
  (princ))

(defun pfp-print-examples ()
  (princ "\nПримеры настройки:")
  (pfp-row "действие" "команда")
  (pfp-row "------------------------------------"
           "--------------------------------------------------------------")
  (pfp-row "включить цвет"           "(setq *pfp-color-mode* T)")
  (pfp-row "вернуть ч/б"             "(setq *pfp-color-mode* nil)")
  (pfp-row "своя папка экспорта"
           "(setq *pfp-output-dir* \"D:/Yandex.Disk/Документы/Файлы PDF\")")
  (pfp-row "чертёж в метрах"         "(setq *pfp-unit-scale* 1000.0)")
  (pfp-row "множитель 1.0"           "(setq *pfp-size-factor* 1.0)")
  (pfp-row "множитель 2.0"           "(setq *pfp-size-factor* 2.0)")
  (pfp-row "ограничить A0"           "(setq *pfp-max-format* \"A0\")")
  (pfp-row "только по объектам"      "(setq *pfp-consider-area* nil)")
  (pfp-row "форс альбомной"          "(setq *pfp-force-orient* \"landscape\")")
  (pfp-row "форс портретной"         "(setq *pfp-force-orient* \"portrait\")")
  (pfp-row "снять форс"              "(setq *pfp-force-orient* nil)")
  (pfp-row "задержка 500 мс"         "(setq *pfp-open-delay* 500)")
  (pfp-row "запасная печать -ПЕЧАТЬ" "(setq *pfp-engine* \"command\")")
  (pfp-row "печать через ActiveX"    "(setq *pfp-engine* \"activex\")")
  (princ))

(defun pfp-print-hints ()
  (princ "\nDWG To PDF.pc3 — ускорение (один раз):")
  (princ "\n  Диспетчер плоттеров -> изменить DWG To PDF.pc3 -> Свойства -> Custom Properties")
  (princ "\n    Raster graphics resolution : 150..200 dpi")
  (princ "\n    Vector graphics resolution : 1200 dpi")
  (princ)
  (princ "\nv3.12: печать через -ПЕЧАТЬ; лента ответов собирается по вкладке")
  (princ "\n  (Модель/Лист). Диалог «размер бумаги не найден» (PAPERUPDATE)")
  (princ "\n  подавляется на время печати. Если ввод кириллической команды")
  (princ "\n  даёт «Неизвестная команда» — используйте псевдоним EXPTPDF")
  (princ "\n  или нажмите Enter — повтор последней команды. Запасной механизм:")
  (princ "\n  (setq *pfp-engine* \"activex\") — печать без командной строки.")
  (princ))

(defun pfp-print-settings ()
  (princ "\nТекущие настройки:")
  (princ (strcat "\n  режим печати : " (pfp-mode-str)
                 " (" (if *pfp-color-mode* *pfp-ctb-color* *pfp-ctb-mono*) ")"))
  (princ (strcat "\n  механизм     : "
                 (if (= *pfp-engine* "command")
                   "-ПЕЧАТЬ (запасной)"
                   "ActiveX (без вопросов)")))
  (princ (strcat "\n  папка        : " (if *pfp-output-dir*
                                         *pfp-output-dir* (pfp-default-dir))))
  (princ (strcat "\n  учёт площади : " (if *pfp-consider-area* "да" "нет")))
  (princ (strcat "\n  множитель    : " (rtos *pfp-size-factor* 2 2)))
  (princ (strcat "\n  масштаб ед.  : " (rtos *pfp-unit-scale* 2 3) " мм"))
  (princ (strcat "\n  макс. формат : " (if *pfp-max-format* *pfp-max-format* "нет")))
  (princ (strcat "\n  форс ориент. : " (if *pfp-force-orient* *pfp-force-orient* "нет")))
  (princ (strcat "\n  задержка     : " (itoa *pfp-open-delay*) " мс"))
  (princ (strcat "\n  способ откр. : " *pfp-open-mode*))
  (princ))

;; ---------------------------------------------------------------------------
;; Список media name
;; ---------------------------------------------------------------------------

(defun pfp-list-media ( / acad doc layout lst v)
  (setq acad   (vlax-get-acad-object))
  (setq doc    (pfp-get acad 'ActiveDocument))
  (setq layout (pfp-get doc  'ActiveLayout))
  (if (or (null layout) (vl-catch-all-error-p layout))
    nil
    (pfp-layout-media-list layout)))

(defun c:ПФПМЕДИА ( / lst nm)
  (setq lst (pfp-list-media))
  (if (null lst)
    (princ "\nСписок недоступен.")
    (progn
      (princ (strcat "\nДоступные media name (всего " (itoa (length lst)) "):"))
      (foreach nm lst
        (princ (strcat "\n  " (vl-princ-to-string nm))))
      (princ)))
  (princ))

;; ---------------------------------------------------------------------------
;; Основная процедура
;; ---------------------------------------------------------------------------

(defun plot-frame-to-pdf-run ( / pt1 pt2 x1 y1 x2 y2 w h w-mm h-mm
                                   dir fname fname-pdf fname-check
                                   fmt media-name short-name orient-str rot-str
                                   n-obj ss-filter ctb-name llw urw ok
                                   old-trans has-trans old-bg old-paper
                                   old-cmdecho )

  (setq has-trans
        (not (vl-catch-all-error-p
               (vl-catch-all-apply 'getvar '("PLOTTRANSPARENCYOVERRIDE")))))
  (if has-trans (setq old-trans (getvar "PLOTTRANSPARENCYOVERRIDE")))
  (setq old-bg      (getvar "BACKGROUNDPLOT"))
  (setq old-paper   (vl-catch-all-apply 'getvar (list "PAPERUPDATE")))
  (setq old-cmdecho (getvar "CMDECHO"))

  ;; Сброс возможного «хвоста» от предыдущих команд
  (pfp-clear-stuck)

  (setvar "BACKGROUNDPLOT" 0)
  ;; Не спрашивать «размер бумаги не найден — использовать по умолчанию?»
  (if (not (vl-catch-all-error-p old-paper)) (setvar "PAPERUPDATE" 1))
  (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" 1))

  (princ "\nЭкспорт области в PDF. Укажите рамку выделения.")
  (setq pt1 (getpoint "\nПервый угол рамки: "))
  (cond
    ((null pt1) (princ "\nОтменено.")
                (pfp-restore-sys has-trans old-trans old-bg old-paper))
    (t
      (setq pt2 (getcorner pt1 "\nВторой угол рамки: "))
      (cond
        ((null pt2) (princ "\nОтменено.")
                    (pfp-restore-sys has-trans old-trans old-bg old-paper))
        (t
          (setq x1 (min (car  pt1) (car  pt2))
                y1 (min (cadr pt1) (cadr pt2))
                x2 (max (car  pt1) (car  pt2))
                y2 (max (cadr pt1) (cadr pt2))
                w  (- x2 x1) h (- y2 y1)
                w-mm (* w *pfp-unit-scale*)
                h-mm (* h *pfp-unit-scale*))

          (setq orient-str (if (pfp-want-landscape w h) "альбомный" "портретный"))

          (setq ss-filter (ssget "_C" (list x1 y1) (list x2 y2)))
          (setq n-obj (if ss-filter (sslength ss-filter) 0))

          (setq fmt        (pfp-pick-format n-obj w-mm h-mm)
                short-name (nth 0 fmt)
                media-name (nth 1 fmt)
                rot-str    (nth 2 fmt))

          (setq ctb-name (if *pfp-color-mode* *pfp-ctb-color* *pfp-ctb-mono*))

          (setq dir       (pfp-output-dir)
                fname     (itoa (pfp-next-number dir))
                fname-pdf (strcat dir "\\" fname))

          ;; Координаты окна для ActiveX: на Модели — WCS,
          ;; на Листе — координаты пространства листа (как указали)
          (if (= 1 (getvar "TILEMODE"))
            (setq llw (trans (list x1 y1) 1 0)
                  urw (trans (list x2 y2) 1 0))
            (setq llw (list x1 y1)
                  urw (list x2 y2)))

          ;; Печать с перехватом ошибок: сообщение не «повисает», а
          ;; печатается целиком (какой бы механизм ни использовался)
          (setq ok (vl-catch-all-apply
                     '(lambda ()
                        (if (= *pfp-engine* "command")
                          (pfp-plot-by-command fname-pdf media-name rot-str
                                               ctb-name x1 y1 x2 y2)
                          (pfp-plot-by-activex media-name short-name rot-str
                                               ctb-name llw urw fname-pdf)))))
          (if (vl-catch-all-error-p ok)
            (progn
              (princ (strcat "\nПФП: ОШИБКА печати — "
                             (vl-catch-all-error-message ok)))
              (setq ok nil))
            (setq ok (not (null ok))))

          (pfp-restore-sys has-trans old-trans old-bg old-paper)

          (setq fname-check
                (cond ((findfile (strcat fname-pdf ".pdf"))
                       (strcat fname-pdf ".pdf"))
                      ((findfile fname-pdf) fname-pdf)
                      (t nil)))

          (if fname-check
            (progn
              (princ (strcat "\nPDF создан: " fname-check
                             " ||| объектов: " (itoa n-obj)
                             " ||| площадь рамки: "
                             (rtos w-mm 2 0) "x" (rtos h-mm 2 0) " мм"
                             " ||| формат: " short-name " " orient-str
                             " ||| область: Рамка"
                             " ||| печать: " (pfp-mode-str)
                             " ||| задержка: " (itoa *pfp-open-delay*) " мс"))
              (princ (strcat "\n  media name: " media-name))
              (pfp-sleep *pfp-open-delay*)
              (pfp-open fname-check))
            (progn
              (princ "\nНе удалось создать PDF (файл не найден после печати).")
              (if (and (= *pfp-engine* "command") ok)
                (princ (strcat "\n  Печать шла через -ПЕЧАТЬ: вероятна рассинхронизация"
                               "\n  ответов (набор вопросов зависит от вкладки Модель/Лист)."
                               "\n  Используйте (setq *pfp-engine* \"activex\").")))))
          (princ))))))

;; ---------------------------------------------------------------------------
;; Очистка
;; ---------------------------------------------------------------------------

(defun plot-frame-to-pdf-clean ( / dir files cnt)
  (setq dir (pfp-output-dir))
  (if (not (vl-file-directory-p dir))
    (princ "\nПапка не существует.")
    (progn
      (setq files (vl-directory-files dir "*.pdf" 1) cnt 0)
      (foreach f files
        (if (vl-file-delete (strcat dir "\\" f))
          (setq cnt (1+ cnt))))
      (princ (strcat "\nУдалено PDF: " (itoa cnt) " в " dir)))))

;; ---------------------------------------------------------------------------
;; Команды
;; ---------------------------------------------------------------------------

(defun c:ЭКСВПДФ  () (plot-frame-to-pdf-run))
(defun c:EXPTPDF  () (plot-frame-to-pdf-run))
(defun c:ОЧИСТПДФ () (plot-frame-to-pdf-clean))
(defun c:ПФПТАБЛ  () (pfp-print-table))
(defun c:ПФПСТАТ  () (pfp-print-settings))

(defun c:ПФПЦВЕТ ()
  (setq *pfp-color-mode* T)
  (princ (strcat "\nПФП: цветная (" *pfp-ctb-color* ").")) (princ))

(defun c:ПФПЧБ ()
  (setq *pfp-color-mode* nil)
  (princ (strcat "\nПФП: ч/б (" *pfp-ctb-mono* ").")) (princ))

(defun c:ПФППАПКА ( / p)
  (setq p (pfp-browse-folder))
  (if p (progn (setq *pfp-output-dir* p)
               (princ (strcat "\nПФП: папка — " p)))
      (princ "\nПФП: выбор отменён."))
  (princ))

(defun c:ПФПТЕМП ()
  (setq *pfp-output-dir* nil)
  (princ (strcat "\nПФП: папка — " (pfp-default-dir)))
  (princ))

;; ---------------------------------------------------------------------------
;; Автозагрузка
;; ---------------------------------------------------------------------------
(princ "\nЗагружено: PlotFrameToPDF.lsp v3.11 — команды: ЭКСВПДФ / EXPTPDF / ОЧИСТПДФ / ПФПТАБЛ / ПФПСТАТ / ПФПМЕДИА / ПФПЦВЕТ / ПФПЧБ / ПФППАПКА / ПФПТЕМП.")
(pfp-print-table)
(pfp-print-vars)
(pfp-print-hints)
(princ)
