;;; -*- coding: windows-1251 -*-
;;; PlotSPDSToPDF.lsp
;;;
;;; Серийная печать выбранных форматов СПДС GraphiCS (SPDSFORMAT)
;;; с сортировкой по листам. Диагностика ЭКССПДСДИАГ — в файле
;;; PlotSPDSDiag.lsp (модуль 2 — загрузить ПОСЛЕ этого файла).
;;;
;;; Команды: ЭКССПДСВПДФ / SPDSTOPDF — серийный экспорт в PDF
;;;          ПСППАПКА / PSPPAPKA — открыть папку журналов
;;;          ЭКССПДСВПДФ / SPDSTOPDF — серийный экспорт в PDF
;;;
;;; Кодировка: CP1251 (ANSI), без BOM.
;;; v0.2: GetBoundingBox через vl-catch-all-apply привязывает к символам
;;;        СЫРОЙ safearray (не variant) — незащищённый vlax-variant-value
;;;        падал с «неверный тип аргумента: variantp». Добавлен
;;;        конвертер psp-va->list (variant -> значение, safearray
;;;        проходит насквозь); разбор строки таблицы диагностики
;;;        обёрнут в перехват — один плохой объект не роняет отчёт.
;;; v0.3: по дампу подтверждено: ObjectName = mcsDbObjectFormat,
;;;        кастомных COM-свойств у объекта НЕТ — формат/лист/листов
;;;        лежат в данных примитива. Добавлен универсальный экстрактор:
;;;        текстовые группы entget (1, 3), бинарные группы (310-312)
;;;        с раскодировкой hex и извлечением читаемых строк, XRecord'ы
;;;        расширения словаря, XData. Автопоиск кандидатов формата.
;;; v0.4: по логу v0.3: у объекта нет ни словаря (HasExtensionDictionary
;;;        = 0), ни текстов в entget — данные формата СПДС держит в своей
;;;        базе. Основной канал — план Б: ТЕКСТЫ штампа в области рамки
;;;        (TEXT/MTEXT/ATTRIB по габаритам объекта, сортировка сверху
;;;        вниз). Добавлен скан текстов; XData (группы 1000+) добавлен
;;;        в экстрактор; исправлен разбор вариантов XRecord.
;;; v0.5: по логу v0.4: HasExtensionDictionary = -1 — словарь ЕСТЬ,
;;;        но XRecord'ов в нём не нашлось (COM-обход не увидел).
;;;        Добавлен глубокий обход словаря расширения через dictnext
;;;        (группа 360 entget; вложенные словари рекурсивно; XRecord'ы
;;;        читаются entget'ом напрямую). Диагностика области расширена:
;;;        все типы объектов в окне, блоки, атрибуты, тексты из
;;;        определений блоков. ПроверкаPlotSPDS: закрытый продукт,
;;;        функциональный ориентир — сортировка/имёнование/многопаг PDF.
;;; v0.6: v0.5 не загрузилась у пользователя («синтаксическая ошибка»),
;;;        хотя строгий сторонний Lisp-ридер читает файл без ошибок.
;;;        Добавлены маркеры прогресса загрузки по секциям — последний
;;;        напечатанный маркер указывает секцию со сбоем.
;;;        Новая команда ПСППАПКА — открывает папку журналов в Проводнике.
;;; v0.7: найдена и исправлена причина сбоя v0.5/v0.6: маркер секции 7
;;;        был вставлен В СЕРЕДИНУ строки комментария, его хвост
;;;        «строки (1,3,1000),» становился кодом — AutoCAD пытался
;;;        вызвать (1,3,1000) как функцию. Комментарий восстановлен.
;;; v0.8: сбой сместился в секцию 8 (большой defun диагностики).
;;;        Диагностика разбита на НЕСКОЛЬКО верхнеуровневых функций
;;;        с маркерами 8a/8b/8c между ними — следующий сбой укажет
;;;        подблок точно. (apply '+ ...) заменён на явную сумму.
;;; Маркер редакции для RELOAD: (ред. 14
;;; v0.14: ПРИЧИНА «stringp T» НАЙДЕНА И ИСПРАВЛЕНА: булевы поля СПДС
;;;        (группа 290) entget отдаёт как T/nil; разбор оставлял такое
;;;        значение как есть, и (atoi T) падал. Разбор 300/40/90/290 теперь
;;;        понимает T->"1", nil->"0" и всё прочее через vl-princ-to-string;
;;;        кратность в экспорте читается со страховкой типа.
;;; v0.13: трассировка локализовала сбой: между «выбрано объектов» и
;;;        сортировкой (цикл сбора). Добавлены контрольные точки [1]-[6]
;;;        по шагам цикла: ssname -> vla-объект -> поля entget -> формат/
;;;        лист -> габариты -> сбор позиции. Последняя напечатанная точка
;;;        укажет оператор, вызывающий «stringp T».
;;; v0.12: по логу первого прогона ЭКССПДСВПДФ: необнаруженная ошибка
;;;        «неверный тип аргумента: stringp T» — добавлена поэтапная
;;;        трассировка печати (объектов/сортировка/папка/каждый лист),
;;;        psp-page-preset обёрнут в vl-catch-all-apply, psp-output-dir
;;;        переживает пустой TEMP. Ошибка диага (тройки вместо строк)
;;;        исправлена в PlotSPDSDiag 0.12.
;;; v0.11: СВОЙСТВА СПДС НАЙДЕНЫ: в entget объекта парами (301 "ключ")
;;;        (300 "значение"): Format, Sheet, SheetCount, Designation,
;;;        Orientation, Multiplicity (журнал 0.10). Новый psp-spds-fields;
;;;        формат/лист/листов в ЭКССПДСВПДФ и диагностике читаются из
;;;        entget (COM-свойства остались запасным каналом). Экстрактор
;;;        собирает пары 301/300 (строки вида "Format: A4"). Кратность
;;;        Multiplicity>1 даёт бумагу на N-1 шагов вверх (как "ФорматXN").
;;; v0.9: ПРИЧИНА СБОЕВ ЗАГРУЗКИ НАЙДЕНА. (load) в AutoCAD не читает
;;;        .lsp дальше ~36 Кбайт (36864 = 0x9000): v0.1-v0.4 (до 36433
;;;        байт) грузились, все версии с v0.5 (45317+) падали с «синтак-
;;;        сической ошибкой» на форме, содержащей этот байт. Диагностика
;;;        вынесена в PlotSPDSDiag.lsp; оба файла теперь меньше предела.
;;;
;;; Этапы разработки:
;;;   0. Диагностика — ЭКССПДСДИАГ: типы объектов, свойства, дамп.
;;;   1. Серийная печать — ЭКССПДСВПДФ: сортировка по «листу»,
;;;      бумага на шаг вверх (A4->A3, A3->A2 ...), файлы s01.pdf ...
;;;   2. Сборка в один многостраничный PDF — следующий этап.

(vl-load-com)

(setq *psp-ver* "0.14")

(setq *psp-open-mode* "rundll")
(setq *psp-open-delay* 300)
(setq *psp-unit-scale* 1.0)
(setq *psp-subdir* "PlotFramePDF")
(setq *psp-color-mode* nil)
(setq *psp-ctb-mono* "monochrome.ctb")
(setq *psp-ctb-color* "acad.ctb")
(setq *psp-output-dir* nil)

;; Имя примитива СПДС и каналы чтения свойств (порядок = приоритет)
(setq *psp-spds-entity* "SPDSFORMAT")

;; Разделы помечены маркерами загрузки (ПСП: секция N/10)
(setq *psp-spds-props-format*
      '("Format" "Формат" "SheetFormat" "FrameFormat" "FormatName"))
(setq *psp-spds-props-sheet*
      '("Sheet" "Лист" "SheetNo" "SheetNumber" "Номер"))
(setq *psp-spds-props-total*
      '("Sheets" "Листов" "SheetsTotal" "TotalSheets"))

;; Таблица форматов (как в PlotFrameToPDF + строка A4)
;; (короткое W_L H_L "портрет" "альбом")
(setq *psp-formats*
  '(("A4"  297  210
     "ISO_full_bleed_A4_(210.00_x_297.00_MM)"
     "ISO_full_bleed_A4_(297.00_x_210.00_MM)")
    ("A3"  420  297
     "ISO_full_bleed_A3_(297.00_x_420.00_MM)"
     "ISO_full_bleed_A3_(420.00_x_297.00_MM)")
    ("A2"  594  420
     "ISO_full_bleed_A2_(420.00_x_594.00_MM)"
     "ISO_full_bleed_A2_(594.00_x_420.00_MM)")
    ("A1"  841  594
     "ISO_full_bleed_A1_(594.00_x_841.00_MM)"
     "ISO_full_bleed_A1_(841.00_x_594.00_MM)")
    ("A0"  1189 841
     "ISO_full_bleed_A0_(841.00_x_1189.00_MM)"
     "ISO_full_bleed_A0_(1189.00_x_841.00_MM)")
    ("2A0" 1682 1189
     "ISO_full_bleed_2A0_(1189.00_x_1682.00_MM)"
     "ISO_full_bleed_2A0_(1682.00_x_1189.00_MM)")
    ("4A0" 2378 1682
     "ISO_full_bleed_4A0_(1682.00_x_2378.00_MM)"
     "ISO_full_bleed_4A0_(2378.00_x_1682.00_MM)")))

;; Сохранённые устройство/формат активного листа (на время печати)
(setq *psp-page-saved* nil)
;; Метка этапа — попадает в сообщение об ошибке
(setq *psp-stage* "-")

;; ---------------------------------------------------------------------------
;; Утилиты
(princ (strcat "\nПСП: секция 1/10 загружена")) (princ)

;; ---------------------------------------------------------------------------

(defun psp-get (obj prop)
  (vl-catch-all-apply 'vlax-get-property (list obj prop)))

(defun psp-safe-put (obj prop val)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 'vlax-put-property (list obj prop val)))))

;; variant -> значение; safearray/прочее -> как есть; nil при ошибке.
(defun psp-va->list-unwrap (x / v)
  (cond
    ((null x) nil)
    ((= (type x) 'VARIANT)
     (setq v (vl-catch-all-apply 'vlax-variant-value (list x)))
     (if (vl-catch-all-error-p v) nil v))
    (t x)))

(defun psp-pad-right (s width / n)
  (setq n (- width (strlen s)))
  (while (> n 0) (setq s (strcat s " ") n (1- n))) s)

(defun psp-default-dir ()
  (strcat (getenv "TEMP") "\\" *psp-subdir*))

(defun psp-output-dir ( / d)
  (if (null *psp-output-dir*)
    (progn
      (setq d (getenv "TEMP"))
      (if (null d) (setq d (getvar "TEMP")))
      (if (null d) (setq d "C:"))
      (setq d (strcat d "\\" *psp-subdir*))
      (vl-mkdir d)
      (setq *psp-output-dir* d)))
  *psp-output-dir*)

(defun psp-clear-stuck ( / doc i)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (setq i 0)
  (while (and (< i 5) (> (getvar "CMDACTIVE") 0))
    (vl-catch-all-apply 'vla-SendCommand (list doc (chr 27)))
    (setq i (1+ i)))
  (princ))

(defun psp-sleep (ms / old-cmdecho)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.delay" ms)
  (setvar "CMDECHO" old-cmdecho))

(defun psp-open-rundll (path / args)
  (setq args (strcat "url.dll,FileProtocolHandler \"" path "\""))
  (startapp "rundll32.exe" args))

(defun psp-open (path)
  (if (= *psp-open-mode* "rundll")
    (psp-open-rundll path)
    (startapp "explorer.exe" path)))

;; Предустановка устройства и формата активного листа через ActiveX:
(princ (strcat "\nПСП: секция 2/10 загружена")) (princ)

;; -ПЕЧАТЬ не вставляет лишний вопрос о неподдерживаемом формате.
;; Прежние значения -> *psp-page-saved* -> psp-page-restore.
(defun psp-page-preset (media-name / doc layout cfg med)
  (setq *psp-page-saved* nil)
  (setq doc (psp-get (vlax-get-acad-object) 'ActiveDocument))
  (setq layout (psp-get doc 'ActiveLayout))
  (if (or (null layout) (vl-catch-all-error-p layout))
    nil
    (progn
      (setq cfg (psp-get layout 'ConfigName)
            med (psp-get layout 'CanonicalMediaName))
      (setq *psp-page-saved*
            (list (if (vl-catch-all-error-p cfg) nil cfg)
                  (if (vl-catch-all-error-p med) nil med)))
      (psp-safe-put layout 'ConfigName "DWG To PDF.pc3")
      (psp-safe-put layout 'CanonicalMediaName media-name))))

(defun psp-page-restore ( / doc layout)
  (if *psp-page-saved*
    (progn
      (setq doc (psp-get (vlax-get-acad-object) 'ActiveDocument))
      (setq layout (psp-get doc 'ActiveLayout))
      (if (and (not (null layout)) (not (vl-catch-all-error-p layout)))
        (progn
          (if (car *psp-page-saved*)
            (psp-safe-put layout 'ConfigName (car *psp-page-saved*)))
          (if (cadr *psp-page-saved*)
            (psp-safe-put layout 'CanonicalMediaName
                          (cadr *psp-page-saved*)))))
      (setq *psp-page-saved* nil)))
  (princ))

;; ---------------------------------------------------------------------------
;; Печать окна через -ПЕЧАТЬ (общая лента ответов, Модель/Лист)
(princ (strcat "\nПСП: секция 3/10 загружена")) (princ)

;; ---------------------------------------------------------------------------

;; Наборы вопросов -ПЕЧАТЬ у Модели и Листа различаются: у Модели нет
;; вопросов «лист последним»/«удалять скрытые», но есть «Задать
;; тонирование» (ответ — ключевое слово). Лента собирается по TILEMODE.
(defun psp-plot-tape (fname media-name rot-str ctb x1 y1 x2 y2
                      / tape old-cmdecho)
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
            "_C"                     ; центрировать
            "_Y"                     ; учитывать стили печати
            ctb                      ; CTB-файл
            "_Y"                     ; учитывать веса линий
            "_N")                    ; не масштабировать веса
      (if (= 1 (getvar "TILEMODE"))
        ;; --- вкладка Модель ---
        (list
          "_As"                      ; тонирование: по отображению
          fname                      ; имя файла
          "_N"                       ; не сохранять в параметры
          "_Y")                      ; печатать
        ;; --- вкладка Лист ---
        (list
          "_N"                       ; не печатать лист последним
          "_N"                       ; не удалять скрытые линии
          fname                      ; имя файла
          "_N"                       ; не сохранять в параметры
          "_Y"))))                   ; печатать
  (apply 'command tape)
  (setvar "CMDECHO" old-cmdecho)
  t)

;; ---------------------------------------------------------------------------
;; СПДС: чтение свойств и геометрии
(princ (strcat "\nПСП: секция 4/10 загружена")) (princ)

;; ---------------------------------------------------------------------------

;; Чтение свойства по списку имён-кандидатов.
;; Возвращает (значение имя_канала) или nil. Значение -> строка.
(defun psp-spds-prop-ex (obj names / nm avail v val)
  (setq val nil)
  (while (and names (null val))
    (setq nm (car names)
          avail (vl-catch-all-apply 'vlax-property-available-p
                                    (list obj nm T)))
    (if (and (not (vl-catch-all-error-p avail)) avail)
      (progn
        (setq v (psp-get obj nm))
        (if (not (vl-catch-all-error-p v))
          (progn
            (setq val (psp-va->list-unwrap v))
            (if (not (null val))
              (setq val (cond
                          ((= (type val) 'STR) (if (= val "") nil val))
                          ((= (type val) 'INT) (itoa val))
                          ((= (type val) 'REAL) (itoa (fix val)))
                          (t (vl-princ-to-string val)))))))))
    (if (null val) (setq names (cdr names))))
  (if val (list val nm) nil))

(defun psp-spds-prop (obj names / r)
  (setq r (psp-spds-prop-ex obj names))
  (if r (car r) nil))

;; Значение hex-символа (0-9, A-F, a-f) или nil.
(defun psp-hexval (ch / c)
  (setq c (ascii ch))
  (cond
    ((and (>= c 48) (<= c 57)) (- c 48))
    ((and (>= c 65) (<= c 70)) (- c 55))
    ((and (>= c 97) (<= c 102)) (- c 87))
    (t nil)))

;; Строка hex-пар -> байтовая строка. Некорректные хвосты игнорируются.
(defun psp-hex->str (hex / i h1 h2 res)
  (setq i 1 res "")
  (if hex
    (progn
      (while (<= (+ i 1) (strlen hex))
        (setq h1 (psp-hexval (substr hex i 1))
              h2 (psp-hexval (substr hex (1+ i) 1)))
        (if (and h1 h2)
          (setq res (strcat res (chr (+ (* 16 h1) h2)))))
        (setq i (+ i 2)))))
  res)

;; Читаемые фрагменты (печатаемые коды 32..255) длиной >= minlen.
(defun psp-printable-runs (s minlen / i code run res)
  (setq i 1 run "" res nil)
  (if s
    (progn
      (while (<= i (strlen s))
        (setq code (ascii (substr s i 1)))
        (if (and (>= code 32) (/= code 127) (<= code 255))
          (setq run (strcat run (substr s i 1)))
          (progn
            (if (>= (strlen run) minlen) (setq res (cons run res)))
            (setq run "")))
        (setq i (1+ i)))
      (if (>= (strlen run) minlen) (setq res (cons run res)))))
  (reverse res))

;; Объединение строк через разделитель.
(defun psp-join (sep lst / res)
  (setq res "")
  (foreach s lst
    (setq res (if (= res "") s (strcat res sep s))))
  res)

;; Строки объекта СПДС: entget (группы 1, 3 и бинарные 310-312),
;; XRecord'ы расширения словаря, XData. Каждый шаг защищён.
(defun psp-spds-strings (obj ent / ed item runs s res dict it nm k301
                             codes vals cl vl i v)
  (setq res nil)
  ;; 1) entget: тексты, бинарные данные, XData (группы 1000+;
  ;;    entget с '("*") возвращает XData всех приложений)
  (setq ed (vl-catch-all-apply 'entget (list ent '("*"))))
  (if (and (not (vl-catch-all-error-p ed)) ed)
    (foreach item ed
      (cond
        ((and (= (car item) 301) (= (type (cdr item)) 'STR))
         (setq k301 (cdr item)))
        ((and k301 (member (car item) '(300 40 90 290)))
         (setq v (cdr item)
               v (cond ((= (type v) 'STR) v)
                       ((= (type v) 'INT) (itoa v))
                       ((= (type v) 'REAL) (rtos v 2 2))
                       ((eq v T) "1")
                       ((null v) "0")
                       (t (vl-princ-to-string v))))
         (setq res (cons (strcat k301 ": " v) res)
               k301 nil))
        ((and (member (car item) '(1 3))
              (= (type (cdr item)) 'STR)
              (> (strlen (cdr item)) 1))
         (setq res (cons (cdr item) res)))
        ((and (member (car item) '(310 311 312))
              (= (type (cdr item)) 'STR))
         (setq runs (psp-printable-runs (psp-hex->str (cdr item)) 4))
         (foreach s runs (setq res (cons s res))))
        ((and (= (car item) 1000) (= (type (cdr item)) 'STR)
              (> (strlen (cdr item)) 1))
         (setq res (cons (cdr item) res)))
        ((and (= (car item) 1004) (= (type (cdr item)) 'STR))
         (setq runs (psp-printable-runs (psp-hex->str (cdr item)) 4))
         (foreach s runs (setq res (cons s res))))
        ((and (>= (car item) 1010) (<= (car item) 1071))
         (setq res (cons (strcat "XD[" (itoa (car item)) "]="
                                 (vl-princ-to-string (cdr item)))
                         res))))))
  ;; 2) расширение словаря: XRecord'ы
  (setq dict (vl-catch-all-apply 'vla-getextensiondictionary
                                 (list obj)))
  (if (and (not (vl-catch-all-error-p dict)) dict)
    (vlax-for it dict
      (setq nm (psp-get it 'ObjectName))
      (if (and (not (vl-catch-all-error-p nm)) (= nm "AcDbXrecord"))
        (progn
          (setq codes nil vals nil)
          (vl-catch-all-apply 'vla-getxrecorddata (list it 'codes 'vals))
          (if (and codes (not (vl-catch-all-error-p codes)))
            (progn
              (setq codes (vl-catch-all-apply 'vlax-safearray->list
                              (list (psp-va->list-unwrap codes))))
              (if (vl-catch-all-error-p codes) (setq codes nil))))
          (if (and vals (not (vl-catch-all-error-p vals)))
            (progn
              (setq vals (vl-catch-all-apply 'vlax-safearray->list
                              (list (psp-va->list-unwrap vals))))
              (if (vl-catch-all-error-p vals) (setq vals nil))))
          (if (and codes vals)
            (progn
              (setq i 0)
              (while (and (< i (length codes))
                          (< i (length vals)))
                (setq v (psp-va->list-unwrap (nth i vals)))
                (if (= (type v) 'SAFEARRAY)
                  (setq v (vl-catch-all-apply 'vlax-safearray->list
                              (list v))))
                (if (vl-catch-all-error-p v) (setq v nil))
                (if (and v (= (type v) 'LIST))
                  (setq v (psp-join " " (mapcar 'vl-princ-to-string v))))
                (if v
                  (setq res (cons (strcat "XR[" (itoa (nth i codes))
                                          "]=" (vl-princ-to-string v))
                                  res)))
                (setq i (1+ i)))))))))
  (reverse res))

;; Замена кириллических гомоглифов на латиницу (остальные символы
;; не трогаются — позиции сохраняются).
(defun psp-spds-latinize (s / i ch res)
  (setq i 1 res "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1)
          res (strcat res
                (cond
                  ((member ch '("А" "A")) "A")
                  ((member ch '("В" "B")) "B")
                  ((member ch '("С" "C")) "C")
                  ((member ch '("Е" "E")) "E")
                  ((member ch '("Н" "H")) "H")
                  ((member ch '("К" "K")) "K")
                  ((member ch '("М" "M")) "M")
                  ((member ch '("О" "O")) "O")
                  ((member ch '("Р" "P")) "P")
                  ((member ch '("Т" "T")) "T")
                  ((member ch '("У" "Y")) "Y")
                  ((member ch '("Х" "X")) "X")
                  (t ch)))
          i (1+ i)))
  res)

;; Кандидаты формата в строках: A1..A5 (латиница/кириллица),
;; 2A0/4A0; кратность вида A4X2/A4Х2 тоже отмечается.
(defun psp-spds-candidates (strs / res s su i ch n1 n2 tok)
  (foreach s strs
    (setq su (psp-spds-latinize (strcase s)) i 1)
    (while (<= i (strlen su))
      (setq ch (substr su i 1))
      (cond
        ;; 2A0 / 4A0
        ((and (member ch '("2" "4"))
              (<= (+ i 2) (strlen su))
              (member (substr su (1+ i) 1) '("A"))
              (= (substr su (+ i 2) 1) "0"))
         (setq tok (strcat ch "A0"))
         (if (not (member tok res)) (setq res (cons tok res)))
         (setq i (+ i 2)))
        ;; A0..A5 (в т.ч. с кратностью A4X3)
        ((member ch '("A"))
         (setq n1 (if (<= (1+ i) (strlen su))
                    (substr su (1+ i) 1) ""))
         (if (and (>= (ascii n1) 48) (<= (ascii n1) 53))
           (progn
             (setq tok (strcat ch n1)
                   n2 (if (<= (+ i 2) (strlen su))
                        (substr su (+ i 2) 1) ""))
             (if (and (member n2 '("X"))
                      (<= (+ i 3) (strlen su))
                      (>= (ascii (substr su (+ i 3) 1)) 49)
                      (<= (ascii (substr su (+ i 3) 1)) 57))
               (setq tok (strcat tok "X" (substr su (+ i 3) 1))))
             (if (not (member tok res)) (setq res (cons tok res))))))
        (t nil))
      (setq i (1+ i))))
  (reverse res))

;; Свойства строки диагностики: (формат-имя лист листов габариты).
;; Пары (301 "ключ") (300 "значение") в entget SPDSFORMAT: Format, Sheet,
;; SheetCount, Designation, Orientation, Multiplicity, Name, Info и т.д.
(defun psp-spds-fields (ent / ed item key val res)
  (setq ed (entget ent) res nil key nil)
  (foreach item ed
    (cond
      ((and (= (car item) 301) (= (type (cdr item)) 'STR))
       (setq key (cdr item)))
      ((and key (member (car item) '(300 40 90 290)))
       (setq val (cdr item)
             val (cond ((= (type val) 'STR) val)
                       ((= (type val) 'INT) (itoa val))
                       ((= (type val) 'REAL) (rtos val 2 2))
                       ((eq val T) "1")
                       ((null val) "0")
                       (t (vl-princ-to-string val))))
       (setq res (cons (cons key val) res) key nil))))
  (reverse res))

(defun psp-field (fields name / p)
  (setq p (assoc name fields))
  (if p (cdr p)))

(defun psp-spds-row (obj / ent f)
  (setq ent (vlax-vla-object->ename obj)
        f (psp-spds-fields ent))
  (list (if (psp-field f "Format")
          (list (psp-field f "Format") "entget301")
          (psp-spds-prop-ex obj *psp-spds-props-format*))
        (if (psp-field f "Sheet")
          (list (psp-field f "Sheet") "entget301")
          (psp-spds-prop-ex obj *psp-spds-props-sheet*))
        (if (psp-field f "SheetCount")
          (list (psp-field f "SheetCount") "entget301")
          (psp-spds-prop-ex obj *psp-spds-props-total*))
        (psp-spds-bbox obj)))

;; Габариты объекта в WCS: (llx lly urx ury) или nil.
(princ (strcat "\nПСП: секция 5/10 загружена")) (princ)

;; GetBoundingBox через vl-catch-all-apply привязывает к символам
;; safearray (иногда variant) — оба варианта разбирает psp-va->list-unwrap.
(defun psp-spds-bbox (obj / mn mx a b)
  (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))
  (if (and (boundp 'mn) (boundp 'mx))
    (progn
      (setq a (vl-catch-all-apply 'vlax-safearray->list
                                  (list (psp-va->list-unwrap mn)))
            b (vl-catch-all-apply 'vlax-safearray->list
                                  (list (psp-va->list-unwrap mx))))
      (if (and (not (vl-catch-all-error-p a))
               (not (vl-catch-all-error-p b))
               a b (= (length a) 3) (= (length b) 3))
        (list (car a) (cadr a) (car b) (cadr b))
        nil))
    nil))

;; Нормализация имени формата: верхний регистр, кириллические
;; гомоглифы -> латиница. "А4х3" -> "A4X3".
(defun psp-spds-norm (s / i ch res)
  (setq s (strcase (vl-string-trim " \\t" s)) res "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1)
          res (strcat res
                (cond
                  ((member ch '("А" "A")) "A")
                  ((member ch '("В" "B")) "B")
                  ((member ch '("С" "C")) "C")
                  ((member ch '("Е" "E")) "E")
                  ((member ch '("Н" "H")) "H")
                  ((member ch '("К" "K")) "K")
                  ((member ch '("М" "M")) "M")
                  ((member ch '("О" "O")) "O")
                  ((member ch '("Р" "P")) "P")
                  ((member ch '("Т" "T")) "T")
                  ((member ch '("У" "Y")) "Y")
                  ((member ch '("Х" "X")) "X")
                  ((member ch '("0" "1" "2" "3" "4"
                                 "5" "6" "7" "8" "9")) ch)
                  (t "")))
          i (1+ i)))
  res)

;; Разбор нормализованного имени: "A4X3" -> ("A4" 3); "2A0" -> ("2A0" 1).
;; База — буквы и цифры до разделителя X; после X — кратность.
(defun psp-spds-parse (norm / k base mult)
  (cond
    ((or (null norm) (= norm "")) nil)
    ((and (setq k (vl-string-search "X" norm)) (> k 0))
     (setq base (substr norm 1 k)
           mult (atoi (substr norm (+ k 2))))
     (if (> mult 0) (list base mult) (list base 1)))
    (t (list norm 1))))

;; Индекс номинала в таблице форматов (по короткому имени).
(defun psp-format-idx (base / i lst found)
  (setq i 0 lst *psp-formats* found nil)
  (while (and lst (not found))
    (if (equal (nth 0 (car lst)) base) (setq found i))
    (setq i (1+ i) lst (cdr lst)))
  found)

;; Бумага «на шаг вверх» по номиналу: A4->A3, A3->A2 ... 4A0->4A0.
;; Возвращает запись *psp-formats* или nil.
(defun psp-spds-size-up (base / idx n)
  (setq idx (psp-format-idx base)
        n (length *psp-formats*))
  (cond
    ((null idx) nil)
    ((>= idx (1- n)) (nth (1- n) *psp-formats*))
    (t (nth (1+ idx) *psp-formats*))))

;; ---------------------------------------------------------------------------
;; СПДС: сортировка и имена
(princ (strcat "\nПСП: секция 6/10 загружена")) (princ)

;; ---------------------------------------------------------------------------

(defun psp-pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n)))

;; Уникальное имя: s01; повторы -> s01_2, s01_3 ...
(defun psp-spds-uniq (base used / nm k)
  (setq nm base k 1)
  (while (member nm used)
    (setq k (1+ k) nm (strcat base "_" (itoa k))))
  (list nm (cons nm used)))

;; Порядок «меньше» для печати: лист по возрастанию; без номера —
;; в конец; внутри — сверху вниз (lly = nth 5, убывает), затем
;; слева направо (llx = nth 4, возрастает).
(defun psp-spds-less (a b)
  (cond
    ((< (nth 3 a) (nth 3 b)) T)
    ((> (nth 3 a) (nth 3 b)) nil)
    ((> (nth 5 a) (nth 5 b)) T)
    ((< (nth 5 a) (nth 5 b)) nil)
    (t (< (nth 4 a) (nth 4 b)))))

;; Сортировка вставкой (списки небольшие).
(defun psp-spds-sorted (items / res it head rest done)
  (setq res nil)
  (foreach it items
    (setq head nil rest res done nil)
    (while (and rest (not done))
      (if (psp-spds-less it (car rest))
        (setq done T)
        (setq head (cons (car rest) head) rest (cdr rest))))
    (setq res (append (reverse head) (list it) rest)))
  res)
(princ (strcat "\nПСП: секция 7/10 загружена")) (princ)
;; ---------------------------------------------------------------------------
;; Команда: ЭКССПДСВПДФ / SPDSTOPDF
(princ (strcat "\nПСП: секция 8/10 загружена")) (princ)

;; ---------------------------------------------------------------------------

(defun c:ЭКССПДСВПДФ ( / ss n i ent obj it items fmt sheet sheet-i
                          bb used rec media rot-str ctb dir stamp fname
                          nm-n w h x1 y1 x2 y2 ok-list skip j row flds mult multv
                          old-bg old-paper old-trans has-trans res)
  (princ "\nЭкспорт форматов СПДС в PDF. Выберите объекты (или All).")
  (setq ss (ssget (list (cons 0 *psp-spds-entity*))))
  (cond
    ((null ss) (princ "\nОтменено.") (princ))
    (t
     ;; Сбор сведений
     (setq n (sslength ss) i 0 items nil skip nil)
     (princ (strcat "\nПСП: выбрано объектов: " (itoa n)))
     (while (< i n)
       (setq ent (ssname ss i))
       (princ "\n  ПСП: [1] имя объекта получено")
       (setq obj (vlax-ename->vla-object ent))
       (princ "\n  ПСП: [2] vla-объект готов")
       (setq flds (psp-spds-fields ent))
       (princ (strcat "\n  ПСП: [3] полей entget: " (itoa (length flds))))
       (setq fmt (psp-field flds "Format"))
       (if (null fmt)
         (setq fmt (psp-spds-prop obj *psp-spds-props-format*)))
       (setq sheet (psp-field flds "Sheet"))
       (if (null sheet)
         (setq sheet (psp-spds-prop obj *psp-spds-props-sheet*)))
       (setq sheet-i (atoi (if sheet sheet "0")))
       (princ (strcat "\n  ПСП: [4] формат=" (if fmt fmt "-")
                      " лист=" (if sheet sheet "-")))
       (setq bb (psp-spds-bbox obj))
       (princ "\n  ПСП: [5] габариты получены")
       ;; кратность: A4 + Multiplicity 3 -> "A4X3" (бумага на 3 шага вверх)
       (setq multv (psp-field flds "Multiplicity"))
       (if (/= (type multv) 'STR) (setq multv "1"))
       (setq mult (atoi multv))
       (if (and fmt (> mult 1))
         (setq fmt (strcat fmt "X" (itoa mult))))
       (if bb
         (setq items
               (cons (list ent obj fmt
                           (if (> sheet-i 0) sheet-i 9999)
                           (car bb) (cadr bb) (caddr bb) (cadddr bb))
                     items))
         (setq skip (cons (cdr (assoc 5 (entget ent))) skip)))
       (princ "\n  ПСП: [6] объект собран")
       (setq i (1+ i)))
     (setq items (psp-spds-sorted (reverse items)))
     (princ (strcat "\nПСП: к печати после сортировки: " (itoa (length items))))
     (cond
       ((null items)
        (princ "\nУ выбранных объектов нет габаритов — печатать нечего.")
        (princ))
       (t
        ;; Папка сборки: %TEMP%\PlotFramePDF\SPDS_<дата>_<время>
        (setq stamp (rtos (getvar "CDATE") 2 6)
              dir (strcat (psp-output-dir)
                          "\\SPDS_" (substr stamp 1 8)
                          "_" (substr stamp 10 6)))
        (if (not (vl-file-directory-p dir)) (vl-mkdir dir))
        (princ (strcat "\nПСП: папка сборки: " dir))
        (setq ctb (if *psp-color-mode* *psp-ctb-color* *psp-ctb-mono*))
        ;; Системные переменные: сохранить -> изменить -> восстановить
        (setq has-trans
              (not (vl-catch-all-error-p
                     (vl-catch-all-apply
                       'getvar '("PLOTTRANSPARENCYOVERRIDE")))))
        (if has-trans (setq old-trans (getvar "PLOTTRANSPARENCYOVERRIDE")))
        (setq old-bg (getvar "BACKGROUNDPLOT"))
        (setq old-paper (vl-catch-all-apply 'getvar (list "PAPERUPDATE")))
        (psp-clear-stuck)
        (setvar "BACKGROUNDPLOT" 0)
        (if (not (vl-catch-all-error-p old-paper)) (setvar "PAPERUPDATE" 1))
        (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" 1))
        ;; Цикл печати
        (setq used nil ok-list nil j 0)
        (foreach it items
          (setq w (- (nth 6 it) (nth 4 it))
                h (- (nth 7 it) (nth 5 it))
                rec (if (nth 2 it)
                      (psp-spds-size-up
                        (car (psp-spds-parse (psp-spds-norm (nth 2 it))))))
                rec (if rec rec (nth 3 *psp-formats*))
                media (if (>= w h) (nth 4 rec) (nth 3 rec))
                rot-str (if (>= w h) "_L" "_P")
                nm-n (psp-spds-uniq
                       (strcat "s" (psp-pad2
                                     (if (< (nth 3 it) 9999)
                                       (nth 3 it)
                                       (1+ j))))
                       used)
                fname (strcat dir "\\" (car nm-n))
                used (cadr nm-n))
          (princ (strcat "\n  ПСП: " (car nm-n) " (" media ")"))
          ;; Предустановка формата листа (без лишних вопросов)
          (setq *psp-stage* "предустановка листа")
          (setq res (vl-catch-all-apply 'psp-page-preset (list media)))
          (if (vl-catch-all-error-p res)
            (progn
              (princ "\n  ПСП: предустановка ОШИБКА: ")
              (princ (vl-catch-all-error-message res))))
          ;; Окно печати задаётся в UCS
          (setq x1 (car (trans (list (nth 4 it) (nth 5 it)) 0 1))
                y1 (cadr (trans (list (nth 4 it) (nth 5 it)) 0 1))
                x2 (car (trans (list (nth 6 it) (nth 7 it)) 0 1))
                y2 (cadr (trans (list (nth 6 it) (nth 7 it)) 0 1)))
          (setq *psp-stage* "печать листа")
          (setq res (vl-catch-all-apply 'psp-plot-tape
                      (list (strcat fname ".pdf") media rot-str ctb
                            x1 y1 x2 y2)))
          (psp-page-restore)
          (if (vl-catch-all-error-p res)
            (princ "  ошибка печати, см. ниже")
            (princ "  отправлено на печать."))
          (if (vl-catch-all-error-p res)
            (progn
              (princ (strcat "\nПСП: ОШИБКА [этап: " *psp-stage*
                             "] — " (vl-catch-all-error-message res)))
              (setq skip (cons (car nm-n) skip)))
            (if (findfile (strcat fname ".pdf"))
              (setq ok-list
                    (cons (list (car nm-n) (nth 2 it) (nth 3 it) media)
                          ok-list))
              (setq skip (cons (car nm-n) skip))))
          (setq j (1+ j)))
        ;; Восстановление системных переменных
        (if has-trans (setvar "PLOTTRANSPARENCYOVERRIDE" old-trans))
        (setvar "BACKGROUNDPLOT" old-bg)
        (if (not (vl-catch-all-error-p old-paper))
          (setvar "PAPERUPDATE" old-paper))
        ;; Отчёт
        (setq ok-list (reverse ok-list))
        (princ (strcat "\nСборка: " dir))
        (princ "\n  | файл   | формат СПДС | лист | бумага PDF    |")
        (princ "\n  |--------|-------------|------|---------------|")
        (foreach row ok-list
          (princ (strcat "\n  | "
                         (psp-pad-right (nth 0 row) 6)
                         " | "
                         (psp-pad-right (if (nth 1 row) (nth 1 row) "-") 11)
                         " | "
                         (if (< (nth 2 row) 9999) (itoa (nth 2 row)) "-")
                         "    | "
                         (psp-pad-right (nth 3 row) 13)
                         " |")))
        (princ (strcat "\nГотово: " (itoa (length ok-list))
                       " из " (itoa (length items))
                       "; без габаритов/ошибок: " (itoa (length skip))))
        (princ "\nСборка в один многостраничный PDF — следующий этап.")
        ;; Открыть папку сборки
        (psp-sleep *psp-open-delay*)
        (psp-open dir)
        (princ)))))
  (princ))
(defun c:SPDSTOPDF () (c:ЭКССПДСВПДФ))
(princ (strcat "\nПСП: секция 9/10 загружена")) (princ)
;; Открыть папку журналов и сборок в Проводнике
(defun c:ПСППАПКА ( / d)
  (setq d (psp-output-dir))
  (if (vl-file-directory-p d)
    (progn
      (startapp "explorer.exe" d)
      (princ (strcat "\nПСП: папка открыта — " d)))
    (princ (strcat "\nПСП: папка не существует — " d)))
  (princ))
(defun c:PSPPAPKA () (c:ПСППАПКА))
(princ (strcat "\nПСП: секция 10/10 загружена")) (princ)
;; ---------------------------------------------------------------------------
;; Загрузка
;; ---------------------------------------------------------------------------

(princ (strcat "\nЗагружено: PlotSPDSToPDF.lsp v" *psp-ver*
               " — команды: ЭКССПДСВПДФ / SPDSTOPDF, ПСППАПКА / PSPPAPKA."))
(princ "\nДиагностика ЭКССПДСДИАГ — в PlotSPDSDiag.lsp: загрузите его следом,")
(princ "\nзатем выполните ЭКССПДСДИАГ и пришлите вывод и spds_diag.txt.")
(princ)