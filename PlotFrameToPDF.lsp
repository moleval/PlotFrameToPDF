;;; -*- coding: windows-1251 -*-
;;; PlotFrameToPDF.lsp
;;;
;;; Экспорт области, выделенной рамкой, в PDF (DWG To PDF.pc3).
;;; Печать через командную строку _.-PLOT с координатами-списками.
;;;
;;; v3.16 относительно v3.10 (база) — только два исправления:
;;; 1) PAPERUPDATE на время печати = 1: подавляет диалог «размер бумаги
;;;    не найден» при первом построении в заново открытом файле
;;;    (сохранение до печати, восстановление на всех выходах).
;;; 2) Лента ответов -ПЕЧАТЬ собирается по вкладке (TILEMODE): наборы
;;;    вопросов Модели и Листа различаются — из-за этого были файлы
;;;    с именем «_N» и оставшийся вопрос «Продолжить построение?».
;;; Рекомендуемая загрузка — через проверяющий загрузчик pfp_loader.lsp
;;; (AutoLispDesign, п.6); допустима и прямая (load ...).
;;;
;;; Команды: ЭКСВПДФ / EXPTPDF / ОЧИСТПДФ / ПФПТАБЛ / ПФПСТАТ / ПФПМЕДИА
;;;          ПФПЦВЕТ / ПФПЧБ / ПФППАПКА / ПФПТЕМП

(vl-load-com)

(setq *pfp-ver* "3.16")

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
     "ISO_full_bleed_2A0_(1189.00_x_1682.00_MM)"
     400)
    ("4A0" 2378 1682
     "ISO_full_bleed_4A0_(1682.00_x_2378.00_MM)"
     "ISO_full_bleed_4A0_(1682.00_x_2378.00_MM)"
     999999)))

;; ---------------------------------------------------------------------------
;; Утилиты
;; ---------------------------------------------------------------------------

(defun pfp-get (obj prop)
  (vl-catch-all-apply 'vlax-get-property (list obj prop)))

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

;; Принудительный сброс зависшего состояния командной строки.
;; Три попытки: Esc через SendCommand, затем пустые Enter, пока CMDACTIVE > 0.
(defun pfp-clear-stuck ( / doc i)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (setq i 0)
  (while (and (< i 5) (> (getvar "CMDACTIVE") 0))
    (vl-catch-all-apply 'vla-SendCommand (list doc (chr 27)))
    (vl-catch-all-apply 'vl-cmdf (list ""))
    (setq i (1+ i)))
  ;; ещё два «холостых» Enter на случай не-CMDACTIVE запросов
  (vl-catch-all-apply 'vl-cmdf (list ""))
  (vl-catch-all-apply 'vl-cmdf (list ""))
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

(defun pfp-print-vars ( / dir-str max-str area-str orient-str)
  (setq dir-str    (if *pfp-output-dir* *pfp-output-dir* "nil (= %TEMP%\\PlotFramePDF)")
        max-str    (if *pfp-max-format* *pfp-max-format* "nil (без ограничения)")
        area-str   (if *pfp-consider-area* "T (учитывать)" "nil (только объекты)")
        orient-str (if *pfp-force-orient* *pfp-force-orient* "nil (авто)"))
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
  (princ))

(defun pfp-print-hints ()
  (princ "\nDWG To PDF.pc3 — ускорение (один раз):")
  (princ "\n  Диспетчер плоттеров -> изменить DWG To PDF.pc3 -> Свойства -> Custom Properties")
  (princ "\n    Raster graphics resolution : 150..200 dpi")
  (princ "\n    Vector graphics resolution : 1200 dpi")
  (princ)
  (princ "\nПеред печатью автоматически сбрасывается зависшее состояние командной строки.")
  (princ "\nСписок media name — команда ПФПМЕДИА.")
  (princ))

(defun pfp-print-settings ()
  (princ "\nТекущие настройки:")
  (princ (strcat "\n  режим печати : " (pfp-mode-str)
                 " (" (if *pfp-color-mode* *pfp-ctb-color* *pfp-ctb-mono*) ")"))
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
    (progn
      (setq lst (vl-catch-all-apply 'vlax-invoke-method
                                    (list layout 'GetCanonicalMediaNames)))
      (if (vl-catch-all-error-p lst) (setq lst nil))
      (if lst (setq v (vl-catch-all-apply 'vlax-variant-value (list lst))))
      (if (and v (not (vl-catch-all-error-p v))) (setq lst v))
      (if (and lst (not (listp lst)))
        (setq lst (vl-catch-all-apply 'vlax-safearray->list (list lst))))
      (if (vl-catch-all-error-p lst) (setq lst nil))
      lst)))

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
;; Основная процедура — печать через -PLOT
;; ---------------------------------------------------------------------------

(defun plot-frame-to-pdf-run ( / pt1 pt2 x1 y1 x2 y2 w h w-mm h-mm
                                   dir fname fname-pdf
                                   fmt media-name short-name orient-str rot-str
                                   n-obj ss-filter ctb-name
                                   old-trans has-trans old-bg old-paper
                                   plot-tape old-cmdecho )

  (setq has-trans
        (not (vl-catch-all-error-p
               (vl-catch-all-apply 'getvar '("PLOTTRANSPARENCYOVERRIDE")))))
  (if has-trans (setq old-trans (getvar "PLOTTRANSPARENCYOVERRIDE")))
  (setq old-bg      (getvar "BACKGROUNDPLOT"))
  ;; PAPERUPDATE: сохраняем, печатаем с 1 (без диалога «размер бумаги
  ;; не найден»), восстанавливаем на всех выходах (см. ниже)
  (setq old-paper   (vl-catch-all-apply 'getvar (list "PAPERUPDATE")))
  (setq old-cmdecho (getvar "CMDECHO"))

  ;; Сброс возможного «хвоста» от предыдущих команд
  (pfp-clear-stuck)

  (setvar "BACKGROUNDPLOT" 0)
  (if (not (vl-catch-all-error-p old-paper)) (setvar "PAPERUPDATE" 1))
  (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" 1))

  (princ "\nЭкспорт области в PDF. Укажите рамку выделения.")
  (setq pt1 (getpoint "\nПервый угол рамки: "))
  (cond
    ((null pt1) (princ "\nОтменено.")
                (setvar "BACKGROUNDPLOT" old-bg)
                (if (not (vl-catch-all-error-p old-paper))
                  (setvar "PAPERUPDATE" old-paper))
                (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" old-trans)))
    (t
      (setq pt2 (getcorner pt1 "\nВторой угол рамки: "))
      (cond
        ((null pt2) (princ "\nОтменено.")
                    (setvar "BACKGROUNDPLOT" old-bg)
                    (if (not (vl-catch-all-error-p old-paper))
                      (setvar "PAPERUPDATE" old-paper))
                    (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" old-trans)))
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

          ;; Печать через -PLOT.
          ;; Наборы вопросов Модели и Листа РАЗЛИЧАЮТСЯ: у Модели нет
          ;; вопросов «лист последним»/«удалять скрытые», но есть
          ;; «Задать тонирование» (ответ — ключевое слово, не Да/Нет).
          ;; Лента собирается по TILEMODE, иначе ответы «съезжают».
          (setvar "CMDECHO" 0)
          (setq plot-tape
            (append
              (list "_.-plot"
                    "_Y"                   ; подробная настройка
                    ""                     ; текущий лист
                    "DWG To PDF.pc3"       ; устройство
                    media-name             ; формат
                    "_M"                   ; мм
                    rot-str                ; ориентация _L / _P
                    "_N"                   ; не переворачивать
                    "_W"                   ; область: Рамка
                    (list x1 y1)           ; нижний левый угол
                    (list x2 y2)           ; верхний правый угол
                    "_F"                   ; вписать
                    "_C"                   ; центрировать
                    "_Y"                   ; учитывать стили печати
                    ctb-name               ; CTB-файл
                    "_Y"                   ; учитывать веса линий
                    "_N")                  ; не масштабировать веса
              (if (= 1 (getvar "TILEMODE"))
                ;; --- вкладка Модель ---
                (list
                  "_As"                    ; тонирование: по отображению
                  fname-pdf                ; имя файла
                  "_N"                     ; не сохранять в параметры
                  "_Y")                    ; печатать
                ;; --- вкладка Лист ---
                (list
                  "_N"                     ; не печатать лист последним
                  "_N"                     ; не удалять скрытые линии
                  fname-pdf                ; имя файла
                  "_N"                     ; не сохранять в параметры
                  "_Y"))))                 ; печатать
          (apply 'command plot-tape)
          (setvar "CMDECHO" old-cmdecho)

          ;; Дождаться полного завершения -PLOT
          (while (> (getvar "CMDACTIVE") 0) (command))

          (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" old-trans))
          (setvar "BACKGROUNDPLOT" old-bg)
          (if (not (vl-catch-all-error-p old-paper))
            (setvar "PAPERUPDATE" old-paper))

          (setq fname-pdf
                (cond ((findfile (strcat fname-pdf ".pdf"))
                       (strcat fname-pdf ".pdf"))
                      ((findfile fname-pdf) fname-pdf)
                      (t nil)))

          (if fname-pdf
            (progn
              (princ (strcat "\nPDF создан: " fname-pdf
                             " ||| объектов: " (itoa n-obj)
                             " ||| площадь рамки: "
                             (rtos w-mm 2 0) "x" (rtos h-mm 2 0) " мм"
                             " ||| формат: " short-name " " orient-str
                             " ||| область: Рамка"
                             " ||| печать: " (pfp-mode-str)
                             " ||| задержка: " (itoa *pfp-open-delay*) " мс"))
              (princ (strcat "\n  media name: " media-name))
              (pfp-sleep *pfp-open-delay*)
              (pfp-open fname-pdf))
            (princ "\nНе удалось создать PDF (файл не найден после -PLOT)."))
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
(princ "\nЗагружено: PlotFrameToPDF.lsp v3.10 — команды: ЭКСВПДФ / EXPTPDF / ОЧИСТПДФ / ПФПТАБЛ / ПФПСТАТ / ПФПМЕДИА / ПФПЦВЕТ / ПФПЧБ / ПФППАПКА / ПФПТЕМП.")
(pfp-print-table)
(pfp-print-vars)
(pfp-print-hints)
(princ)