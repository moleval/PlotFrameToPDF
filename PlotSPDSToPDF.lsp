;;; -*- coding: windows-1251 -*-
;;; PlotSPDSToPDF.lsp
;;;
;;; Экспорт объектов форматов СПДС GraphiCS (SPDSFORMAT) в PDF.
;;; Диагностика объектов и каналов чтения свойств + серийная печать
;;; выбранных форматов с сортировкой по листам.
;;;
;;; Команды: ЭКССПДСДИАГ / SPDSDIAG — диагностика SPDSFORMAT
;;;          ЭКССПДСВПДФ / SPDSTOPDF — серийный экспорт в PDF
;;;
;;; Кодировка: CP1251 (ANSI), без BOM.
;;; v0.2: GetBoundingBox через vl-catch-all-apply привязывает к символам
;;;        СЫРОЙ safearray (не variant) — незащищённый vlax-variant-value
;;;        падал с «неверный тип аргумента: variantp». Добавлен
;;;        конвертер psp-va->list (variant -> значение, safearray
;;;        проходит насквозь); разбор строки таблицы диагностики
;;;        обёрнут в перехват — один плохой объект не роняет отчёт.
;;; Маркер редакции для RELOAD: (ред. 2
;;;
;;; Этапы разработки:
;;;   0. Диагностика — ЭКССПДСДИАГ: типы объектов, свойства, дамп.
;;;   1. Серийная печать — ЭКССПДСВПДФ: сортировка по «листу»,
;;;      бумага на шаг вверх (A4->A3, A3->A2 ...), файлы s01.pdf ...
;;;   2. Сборка в один многостраничный PDF — следующий этап.

(vl-load-com)

(setq *psp-ver* "0.2")

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
  (setq d (if *psp-output-dir* *psp-output-dir* (psp-default-dir)))
  (if (not (vl-file-directory-p d)) (vl-mkdir d))
  d)

;; Сброс зависшего состояния командной строки: только Esc, пока
;; CMDACTIVE > 0. Пустые Enter не отправляются (Enter на пустой строке
;; повторяет последнюю команду — вложенный самозапуск).
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
;; ---------------------------------------------------------------------------

;; Наборы вопросов -ПЕЧАТЬ у Модели и Листа различаются: у Модели нет
;; вопросов «лист последним»/«удалять скрытые», но есть «Задать
;; тонирование» (ответ — ключевое слово). Лента собирается по TILEMODE.
(defun psp-plot-tape (fname media-name rot-str ctb x1 y1 x2 y2
                      / tape old-cmdecho i)
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
  (setq i 0)
  (while (and (> (getvar "CMDACTIVE") 0) (< i 15))
    (command "")
    (setq i (1+ i)))
  t)

;; ---------------------------------------------------------------------------
;; СПДС: чтение свойств и геометрии
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

;; Свойства строки диагностики: (формат-имя лист листов габариты).
(defun psp-spds-row (obj)
  (list (psp-spds-prop-ex obj *psp-spds-props-format*)
        (psp-spds-prop-ex obj *psp-spds-props-sheet*)
        (psp-spds-prop-ex obj *psp-spds-props-total*)
        (psp-spds-bbox obj)))

;; Габариты объекта в WCS: (llx lly urx ury) или nil.
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

;; ---------------------------------------------------------------------------
;; Команда: ЭКССПДСДИАГ / SPDSDIAG
;; ---------------------------------------------------------------------------

(defun c:ЭКССПДСДИАГ ( / ss n i ent obj hnd doc ms e nm counts key
                          fmt sheet total bb row f)
  (princ "\nСПДС-диагностика. Выберите объекты форматов (или All).")
  (setq ss (ssget (list (cons 0 *psp-spds-entity*))))
  (cond
    ((null ss)
     (princ (strcat "\nОбъектов " *psp-spds-entity* " в выборе нет."))
     (princ "\nСканирую модель по типам объектов...")
     (setq doc (psp-get (vlax-get-acad-object) 'ActiveDocument)
           ms (psp-get doc 'ModelSpace)
           counts nil)
     (if (and (not (null ms)) (not (vl-catch-all-error-p ms)))
       (progn
         (vlax-for e ms
           (setq nm (psp-get e 'ObjectName))
           (if (and (not (vl-catch-all-error-p nm)) (= (type nm) 'STR))
             (if (assoc nm counts)
               (setq counts (subst (cons nm (1+ (cdr (assoc nm counts))))
                                   (assoc nm counts) counts))
               (setq counts (cons (cons nm 1) counts)))))
         (princ "\nТипы объектов в модели (имя | количество):")
         (foreach key counts
           (princ (strcat "\n  " (psp-pad-right (car key) 32)
                          " | " (itoa (cdr key)))))
         (princ "\nЕсли среди типов есть имя с FORMAT/СПДС — сообщите его:")
         (princ "\nфильтр подстроится переменной *psp-spds-entity*."))
       (princ "\nМодель недоступна для сканирования."))
     (princ))
    (t
     (setq n (sslength ss) i 0)
     (princ (strcat "\nНайдено объектов: " (itoa n)))
     (princ "\n  | объект    | формат    | канал       | лист | листов | окно |")
     (princ "\n  |-----------|-----------|-------------|------|--------|------|")
     (while (< i n)
       (setq ent (ssname ss i)
             obj (vlax-ename->vla-object ent)
             hnd (cdr (assoc 5 (entget ent)))
             row (vl-catch-all-apply 'psp-spds-row (list obj)))
       (if (vl-catch-all-error-p row)
         (princ (strcat "\n  | " (psp-pad-right hnd 9)
                        " | ОШИБКА: "
                        (vl-catch-all-error-message row)))
         (progn
           (setq fmt (nth 0 row)
                 sheet (nth 1 row)
                 total (nth 2 row)
                 bb (nth 3 row))
           (princ (strcat "\n  | " (psp-pad-right hnd 9)
                          " | " (psp-pad-right (if fmt (car fmt) "-") 9)
                          " | " (psp-pad-right (if fmt (cadr fmt) "-") 11)
                          " | " (psp-pad-right (if sheet (car sheet) "-") 4)
                          " | " (psp-pad-right (if total (car total) "-") 6)
                          " | " (if bb "да" "НЕТ") " |"))))
       (setq i (1+ i)))
     ;; Дамп первого объекта: командная строка + файл журнала
     (setq ent (ssname ss 0)
           obj (vlax-ename->vla-object ent))
     (princ "\n--- Свойства первого объекта (vlax-dump-object): ---")
     (vl-catch-all-apply 'vlax-dump-object (list obj))
     (setq f (open (strcat (psp-output-dir) "\\spds_diag.txt") "a"))
     (if f
       (progn
         (write-line "---- SPDSFORMAT ----" f)
         (write-line (vl-prin1-to-string (entget ent '("*"))) f)
         (close f)
         (princ (strcat "\nДанные entget дописаны: "
                        (psp-output-dir) "\\spds_diag.txt")))
       (princ "\nФайл журнала недоступен для записи."))
     (princ "\nЕсли формат/лист показаны «-», пришлите весь вывод команды.")
     (princ)))
  (princ))

(defun c:SPDSDIAG () (c:ЭКССПДСДИАГ))

;; ---------------------------------------------------------------------------
;; Команда: ЭКССПДСВПДФ / SPDSTOPDF
;; ---------------------------------------------------------------------------

(defun c:ЭКССПДСВПДФ ( / ss n i ent obj it items fmt sheet sheet-i
                          bb used rec media rot-str ctb dir stamp fname
                          nm-n w h x1 y1 x2 y2 ok-list skip j row
                          old-bg old-paper old-trans has-trans res)
  (princ "\nЭкспорт форматов СПДС в PDF. Выберите объекты (или All).")
  (setq ss (ssget (list (cons 0 *psp-spds-entity*))))
  (cond
    ((null ss) (princ "\nОтменено.") (princ))
    (t
     ;; Сбор сведений
     (setq n (sslength ss) i 0 items nil skip nil)
     (while (< i n)
       (setq ent (ssname ss i)
             obj (vlax-ename->vla-object ent)
             fmt (psp-spds-prop obj *psp-spds-props-format*)
             sheet (psp-spds-prop obj *psp-spds-props-sheet*)
             sheet-i (atoi (if sheet sheet "0"))
             bb (psp-spds-bbox obj))
       (if bb
         (setq items
               (cons (list ent obj fmt
                           (if (> sheet-i 0) sheet-i 9999)
                           (car bb) (cadr bb) (caddr bb) (cadddr bb))
                     items))
         (setq skip (cons (cdr (assoc 5 (entget ent))) skip)))
       (setq i (1+ i)))
     (setq items (psp-spds-sorted (reverse items)))
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
          ;; Предустановка формата листа (без лишних вопросов)
          (setq *psp-stage* "предустановка листа")
          (psp-page-preset media)
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

;; ---------------------------------------------------------------------------
;; Загрузка
;; ---------------------------------------------------------------------------

(princ (strcat "\nЗагружено: PlotSPDSToPDF.lsp v" *psp-ver*
               " — команды: ЭКССПДСДИАГ / SPDSDIAG, ЭКССПДСВПДФ / SPDSTOPDF."))
(princ "\nСначала выполните ЭКССПДСДИАГ и пришлите вывод — по нему")
(princ "\nуточняются каналы чтения свойств СПДС.")
(princ)
