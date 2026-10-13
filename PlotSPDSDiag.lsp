;;; -*- coding: windows-1251 -*-
;;; PlotSPDSDiag.lsp
;;;
;;; Диагностика объектов форматов СПДС GraphiCS (SPDSFORMAT) — модуль 2 из 2.
;;; Требует предварительно загруженного PlotSPDSToPDF.lsp (использует
;;; его функции psp-*). Команда ЭКССПДСДИАГ / SPDSDIAG — отчёт по объекту:
;;; свойства, строки entget, словарь расширения, тексты штампа в окне,
;;; журнал %TEMP%\PlotFramePDF\spds_diag.txt (открыть: ПСППАПКА).
;;;
;;; Кодировка: CP1251 (ANSI), без BOM.
;;; v0.9: выделен из PlotSPDSToPDF.lsp: (load) в AutoCAD не читает .lsp
;;;        дальше ~36 Кбайт (36864 = 0x9000), поэтому диагностика вынесена
;;;        в отдельный файл. Прогресс загрузки: маркеры «ПСПД: секция N/6».
;;; v0.12: фикс «неверный тип аргумента: stringp (550.0 3784.52 ...)»
;;;        после «Текстов с учетом атрибутов и блоков»: тексты из отчета
;;;        по окну приходят тройками (x y "строка") — в список кандидатов
;;;        попадает только строка (третий элемент).
;;; v0.11: исправлена ошибка этапа 3 («неверный тип аргумента: stringp»):
;;;        группа 360 entget содержит ИМЯ словаря (ename), а не хэндл —
;;;        передаётся в psp-dict-walk напрямую, без handent.
;;; v0.10: psp-diag-window пересобран консервативно (плоская структура,
;;;        два компактных defun, без ё/тире в строках, без (if ...) внутри
;;;        strcat): именно на нём читатель спотыкался (ПСПД 3 печаталась,
;;;        дальше «синтаксическая ошибка»; 19 Кб — предел 36 Кб опровергнут).
;;;        Новая секция-маркер ПСПД 3a между двумя функциями этапа 5.
;;; Маркер редакции для RELOAD: (ред. 12

(if (null *psp-ver*)
  (princ "\nПСПД: ВНИМАНИЕ — сначала загрузите PlotSPDSToPDF.lsp!"))

;; Тексты (TEXT/MTEXT/ATTRIB) в области окна. Возвращает список строк,
;; отсортированный сверху вниз, затем слева направо.
(defun psp-text-yless (a b)
  (cond
    ((> (car a) (car b)) T)
    ((< (car a) (car b)) nil)
    (t (< (cadr a) (cadr b)))))

(defun psp-text-sorted (items / res it head rest done)
  (setq res nil)
  (foreach it items
    (setq head nil rest res done nil)
    (while (and rest (not done))
      (if (psp-text-yless it (car rest))
        (setq done T)
        (setq head (cons (car rest) head) rest (cdr rest))))
    (setq res (append (reverse head) (list it) rest)))
  res)

(defun psp-texts-in-window (p1 p2 / ss n i e ed tp s pt res)
  (setq res nil)
  (setq ss (vl-catch-all-apply 'ssget
             (list "_C" p1 p2 '((0 . "TEXT,MTEXT,ATTRIB")))))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq e (ssname ss i)
              ed (entget e)
              tp (cdr (assoc 0 ed))
              s "")
        (cond
          ((or (= tp "TEXT") (= tp "ATTRIB"))
           (setq s (cdr (assoc 1 ed))))
          ((= tp "MTEXT")
           (foreach gc ed
             (if (and (member (car gc) '(1 3))
                      (= (type (cdr gc)) 'STR))
               (setq s (strcat s (cdr gc)))))))
        (if (and s (/= s ""))
          (progn
            (setq pt (cdr (assoc 10 ed)))
            ;; точка вставки: (x y z) -> элемент (y x строка)
            (setq res (cons (list (cadr pt) (car pt) s) res))))
        (setq i (1+ i)))))
  (mapcar 'psp-text-str (psp-text-sorted (reverse res))))

(defun psp-text-str (it) (nth 2 it))
;; Извлечение читаемых строк из entget XRecord'а: строки (1,3,1000),
;; бинарные (310-312, 1004) через hex, числа печатает vl-princ-to-string.
(defun psp-xrecord-strings (ed / item runs res)
  (setq res nil)
  (foreach item ed
    (cond
      ((and (member (car item) '(1 3 1000))
            (= (type (cdr item)) 'STR)
            (> (strlen (cdr item)) 1))
       (setq res (cons (cdr item) res)))
      ((and (member (car item) '(310 311 312 1004))
            (= (type (cdr item)) 'STR))
       (setq runs (psp-printable-runs (psp-hex->str (cdr item)) 4))
       (foreach s runs (setq res (cons s res))))))
  (reverse res))

;; Глубокий обход словаря (ename): печатает структуру, собирает строки.
;; depth — уровень вложенности; рекурсия в ACDBDICTIONARY до 4.
(defun psp-dict-walk (dict-ename prefix depth / item en ed tp key strs
                                      nm sub)
  (setq strs nil)
  (if (< depth 4)
    (progn
      (setq item (dictnext dict-ename T))
      (setq nm 0)
      (while (and item (< nm 200))
        (setq en (cdr (assoc -1 item))
              key (cdr (assoc 3 item))
              ed (entget en)
              tp (cdr (assoc 0 ed)))
        (princ (strcat "\n  " prefix key " : " tp))
        (cond
          ((= (strcase tp) "XRECORD")
           (foreach s (psp-xrecord-strings ed)
             (princ (strcat "\n    > " s))
             (setq strs (cons s strs))))
          ((= (strcase tp) "ACDBDICTIONARY")
           (foreach s (psp-dict-walk en (strcat prefix key "/")
                                     (1+ depth))
             (setq strs (cons s strs))))
          ((and (= (type (cdr (assoc 1 ed))) 'STR)
                (> (strlen (cdr (assoc 1 ed))) 1))
           (princ (strcat "\n    > " (cdr (assoc 1 ed))))
           (setq strs (cons (cdr (assoc 1 ed)) strs))))
        (setq item (dictnext dict-ename nil)
              nm (1+ nm)))))
  (reverse strs))

;; Отчёт по области окна БЕЗ фильтра: типы объектов, блоки, атрибуты,
;; тексты из определений блоков. Возвращает (counts inserts texts).
(defun psp-block-def-texts (blkname / rec e ed tp s pt res)
  (setq res nil rec (tblsearch "BLOCK" blkname))
  (if rec
    (progn
      (setq e (cdr (assoc -2 rec)))
      (while e
        (setq ed (entget e)
              tp (cdr (assoc 0 ed))
              s "")
        (cond
          ((or (= tp "TEXT") (= tp "ATTRIB"))
           (setq s (cdr (assoc 1 ed))))
          ((= tp "MTEXT")
           (foreach gc ed
             (if (and (member (car gc) '(1 3))
                      (= (type (cdr gc)) 'STR))
               (setq s (strcat s (cdr gc)))))))
        (if (and s (/= s ""))
          (progn
            (setq pt (cdr (assoc 10 ed)))
            (setq res (cons (list (cadr pt) (car pt) s) res))))
        (setq e (entnext e)))))
  (reverse res))

;; Атрибуты вставки: список строк (y x текст).
(defun psp-insert-attribs (ins-ename / e ed res pt)
  (setq res nil
        e (entnext ins-ename))
  (while (and e
              (setq ed (entget e))
              (/= (cdr (assoc 0 ed)) "SEQEND"))
    (if (= (cdr (assoc 0 ed)) "ATTRIB")
      (progn
        (setq pt (cdr (assoc 10 ed)))
        (setq res (cons (list (cadr pt) (car pt)
                              (cdr (assoc 1 ed))) res))))
    (setq e (entnext e)))
  (reverse res))

;; Счётчик: (тип . кол) в assoc-списке.
(defun psp-count-inc (lst key / pair)
  (if (assoc key lst)
    (subst (cons key (1+ (cdr (assoc key lst)))) (assoc key lst) lst)
    (cons (cons key 1) lst)))

;; Полный отчёт по окну: (counts inserts texts)
(defun psp-window-report (p1 p2 / ss n i ed tp counts ins-names atts
                              def-texts seen txts)
  (setq counts nil ins-names nil seen nil txts nil)
  (setq ss (vl-catch-all-apply 'ssget (list "_C" p1 p2)))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq ed (entget (ssname ss i))
              tp (cdr (assoc 0 ed))
              counts (psp-count-inc counts tp))
        (if (= tp "INSERT")
          (progn
            (setq ins-names
                  (psp-count-inc ins-names (cdr (assoc 2 ed))))
            (if (< (length txts) 200)
              (progn
                (setq atts (psp-insert-attribs (ssname ss i)))
                (foreach it atts (setq txts (cons it txts)))))))
        (setq i (1+ i)))
      ;; Тексты из определений уникальных блоков
      (foreach bn ins-names
        (if (and (< (length seen) 20)
                 (not (member (car bn) seen)))
          (progn
            (setq seen (cons (car bn) seen))
            (setq def-texts (psp-block-def-texts (car bn)))
            (foreach it def-texts
              (if (< (length txts) 200) (setq txts (cons it txts)))))))))
  (list counts ins-names txts))
(princ (strcat "\nПСПД: секция 1/6 загружена")) (princ)
;; ---------------------------------------------------------------------------
;; Команда: ЭКССПДСДИАГ / SPDSDIAG
;; ---------------------------------------------------------------------------

;; Данные диагностики передаются через переменные *psp-d-*
(setq *psp-d-ss* nil *psp-d-n* 0 *psp-d-i* 0
      *psp-d-ent* nil *psp-d-obj* nil *psp-d-hnd* ""
      *psp-d-fmt* nil *psp-d-sheet* nil *psp-d-total* nil
      *psp-d-bb* nil *psp-d-strs* nil *psp-d-cands* nil
      *psp-d-edd* nil *psp-d-dictstr* nil *psp-d-dictc* nil
      *psp-d-txts* nil *psp-d-txtc* nil *psp-d-wr* nil
      *psp-d-alltext* nil *psp-d-nm* nil)

;; Сумма счётчиков assoc-списка (тип . количество)
(defun psp-sum-counts (lst / s)
  (setq s 0)
  (foreach p lst (setq s (+ s (cdr p))))
  s)

;; Этап 1: выбор объектов и таблица формат/лист/листов/окно
(defun psp-diag-table ( / row)
  (princ "\nСПДС-диагностика. Выберите объекты форматов (или All).")
  (setq *psp-d-ss* (ssget (list (cons 0 *psp-spds-entity*))))
  (cond
    ((null *psp-d-ss*)
     (princ (strcat "\nОбъектов " *psp-spds-entity* " в выборе нет."))
     (princ "\nСканирую модель по типам объектов...")
     (psp-diag-model-types))
    (t
     (setq *psp-d-n* (sslength *psp-d-ss*) *psp-d-i* 0)
     (princ (strcat "\nНайдено объектов: " (itoa *psp-d-n*)))
     (princ "\n  | объект    | формат    | канал       | лист | листов | окно |")
     (princ "\n  |-----------|-----------|-------------|------|--------|------|")
     (while (< *psp-d-i* *psp-d-n*)
       (setq *psp-d-ent* (ssname *psp-d-ss* *psp-d-i*)
             *psp-d-obj* (vlax-ename->vla-object *psp-d-ent*)
             *psp-d-hnd* (cdr (assoc 5 (entget *psp-d-ent*)))
             row (vl-catch-all-apply 'psp-spds-row
                   (list *psp-d-obj*)))
       (if (vl-catch-all-error-p row)
         (princ (strcat "\n  | " (psp-pad-right *psp-d-hnd* 9)
                        " | ОШИБКА: "
                        (vl-catch-all-error-message row)))
         (progn
           (setq *psp-d-fmt* (nth 0 row)
                 *psp-d-sheet* (nth 1 row)
                 *psp-d-total* (nth 2 row)
                 *psp-d-bb* (nth 3 row))
           (princ (strcat "\n  | " (psp-pad-right *psp-d-hnd* 9)
                          " | " (psp-pad-right
                                  (if *psp-d-fmt* (car *psp-d-fmt*) "-") 9)
                          " | " (psp-pad-right
                                  (if *psp-d-fmt* (cadr *psp-d-fmt*) "-") 11)
                          " | " (psp-pad-right
                                  (if *psp-d-sheet*
                                    (car *psp-d-sheet*) "-") 4)
                          " | " (psp-pad-right
                                  (if *psp-d-total*
                                    (car *psp-d-total*) "-") 6)
                          " | " (if *psp-d-bb* "да" "НЕТ") " |"))))
       (setq *psp-d-i* (1+ *psp-d-i*)))
     ;; Первый объект — кандидат на глубокую диагностику
     (setq *psp-d-ent* (ssname *psp-d-ss* 0)
           *psp-d-obj* (vlax-ename->vla-object *psp-d-ent*)))))

;; Этап 1а: скан типов объектов модели (когда выбор пуст)
(defun psp-diag-model-types ( / doc ms e nm key counts)
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

;; Этап 2: дамп объекта и универсальный экстрактор данных
(defun psp-diag-extract ()
  (princ "\n--- Свойства и методы первого объекта (vlax-dump-object T): ---")
  (vl-catch-all-apply 'vlax-dump-object (list *psp-d-obj* T))
  (setq *psp-d-nm* (psp-get *psp-d-obj* 'ObjectName)
        *psp-d-strs* (vl-catch-all-apply 'psp-spds-strings
                         (list *psp-d-obj* *psp-d-ent*)))
  (if (vl-catch-all-error-p *psp-d-strs*)
    (progn
      (princ "\nЭкстрактор данных: ОШИБКА — ")
      (princ (vl-catch-all-error-message *psp-d-strs*))
      (setq *psp-d-strs* nil))
    (progn
      (setq *psp-d-strs* (reverse *psp-d-strs*)
            *psp-d-cands* (psp-spds-candidates *psp-d-strs*))))
  (princ (strcat "\nObjectName: "
                 (if (and *psp-d-nm* (not (vl-catch-all-error-p *psp-d-nm*)))
                   *psp-d-nm* "-")))
  (princ (strcat "\nИзвлечено строк данных: "
                 (itoa (length *psp-d-strs*))))
  (princ (strcat "\nКандидаты формата: "
                 (if *psp-d-cands*
                   (psp-join ", " *psp-d-cands*) "не найдены")))
  (princ "\n--- Извлечённые строки (первые 40): ---")
  (setq *psp-d-i* 0)
  (foreach s *psp-d-strs*
    (if (< *psp-d-i* 40) (princ (strcat "\n  > " s)))
    (setq *psp-d-i* (1+ *psp-d-i*)))
  (princ))
(princ (strcat "\nПСПД: секция 2/6 загружена")) (princ)
;; Этап 3: глубокий обход словаря расширения (группа 360)
(defun psp-diag-dict ()
  (setq *psp-d-edd*
        (vl-catch-all-apply 'entget (list *psp-d-ent* '("*")))
        *psp-d-dictstr* nil *psp-d-dictc* nil)
  (if (and (not (vl-catch-all-error-p *psp-d-edd*))
           (assoc 360 *psp-d-edd*))
    (progn
      (princ "\n--- Словарь расширения (группа 360): ---")
      (setq *psp-d-dictstr*
            (vl-catch-all-apply 'psp-dict-walk
              (list (cdr (assoc 360 *psp-d-edd*)) "" 0)))
      (if (vl-catch-all-error-p *psp-d-dictstr*)
        (progn
          (princ "\n  ОШИБКА обхода: ")
          (princ (vl-catch-all-error-message *psp-d-dictstr*))
          (setq *psp-d-dictstr* nil))
        (progn
          (setq *psp-d-dictc*
                (psp-spds-candidates *psp-d-dictstr*))
          (princ (strcat "\nСтрок из словаря: "
                         (itoa (length *psp-d-dictstr*))))
          (princ (strcat "\nКандидаты формата в словаре: "
                         (if *psp-d-dictc*
                           (psp-join ", " *psp-d-dictc*) "не найдены")))
          (setq *psp-d-i* 0)
          (foreach s *psp-d-dictstr*
            (if (< *psp-d-i* 40) (princ (strcat "\n  D> " s)))
            (setq *psp-d-i* (1+ *psp-d-i*))))))
    (princ "\nСловаря расширения в entget нет (группы 360)."))
  (princ))
;; Этап 4: тексты штампа в области рамки
(defun psp-diag-texts ()
  (setq *psp-d-txts* nil *psp-d-txtc* nil)
  (if *psp-d-bb*
    (progn
      (setq *psp-d-txts*
            (vl-catch-all-apply 'psp-texts-in-window
              (list (list (car *psp-d-bb*) (cadr *psp-d-bb*))
                    (list (caddr *psp-d-bb*) (cadddr *psp-d-bb*)))))
      (if (vl-catch-all-error-p *psp-d-txts*)
        (progn
          (princ "\nСкан текстов: ОШИБКА — ")
          (princ (vl-catch-all-error-message *psp-d-txts*))
          (setq *psp-d-txts* nil))
        (progn
          (setq *psp-d-txtc*
                (psp-spds-candidates *psp-d-txts*))
          (princ (strcat "\nТекстов в области рамки: "
                         (itoa (length *psp-d-txts*))))
          (princ "\n--- Тексты (первые 40, сверху вниз): ---")
          (setq *psp-d-i* 0)
          (foreach s *psp-d-txts*
            (if (< *psp-d-i* 40) (princ (strcat "\n  > " s)))
            (setq *psp-d-i* (1+ *psp-d-i*)))
          (princ (strcat "\nКандидаты формата в текстах: "
                         (if *psp-d-txtc*
                           (psp-join ", " *psp-d-txtc*) "не найдены")))))))
  (princ))
(princ (strcat "\nПСПД: секция 3/6 загружена")) (princ)
;; Этап 5: расширенный отчёт по области (типы, блоки, атрибуты)
;; Этап 5: расширенный отчёт по области (типы, блоки, атрибуты).
;; Пересобран консервативно: два компактных defun, плоская структура,
;; без (if ...) внутри strcat — читатель Visual LISP спотыкался здесь.
(defun psp-diag-window ()
  (setq *psp-d-wr* nil)
  (setq *psp-d-alltext* nil)
  (if *psp-d-bb*
    (progn
      (setq *psp-d-wr*
            (vl-catch-all-apply 'psp-window-report
              (list (list (car *psp-d-bb*) (cadr *psp-d-bb*))
                    (list (caddr *psp-d-bb*) (cadddr *psp-d-bb*)))))
      (if (vl-catch-all-error-p *psp-d-wr*)
        (progn
          (princ "\nОтчет по окну: ОШИБКА: ")
          (princ (vl-catch-all-error-message *psp-d-wr*))
          (setq *psp-d-wr* nil))
        (psp-diag-window-report))))
  (princ))

(princ (strcat "\nПСПД: секция 3a/6 загружена")) (princ)

(defun psp-diag-window-report ( / key)
  (princ "\n--- Объекты в окне по типам ---")
  (foreach key (car *psp-d-wr*)
    (princ (strcat "\n  " (psp-pad-right (car key) 24)
                   " | " (itoa (cdr key)))))
  (princ "\n--- Блоки (имя | количество) ---")
  (foreach key (cadr *psp-d-wr*)
    (princ (strcat "\n  " (psp-pad-right (car key) 24)
                   " | " (itoa (cdr key)))))
  (setq *psp-d-alltext*
        (append *psp-d-txts*
                (mapcar '(lambda (it) (if (= (type it) 'STR) it (nth 2 it)))
                        (nth 2 *psp-d-wr*))))
  (princ (strcat "\nТекстов с учетом атрибутов и блоков: "
                 (itoa (length *psp-d-alltext*))))
  (setq *psp-d-txtc* (psp-spds-candidates *psp-d-alltext*))
  (princ "\nКандидаты формата (все источники): ")
  (if *psp-d-txtc*
    (princ (psp-join ", " *psp-d-txtc*))
    (princ "не найдены"))
  (if (null *psp-d-alltext*)
    (progn
      (princ "\nЕсли штамп видим, но текстов 0: проверьте, что все")
      (princ "\n  слои разморожены и включены; ssget не видит")
      (princ "\n  примитивы на выключенных или замороженных слоях.")))
  (princ))
(princ (strcat "\nПСПД: секция 4/6 загружена")) (princ)
;; Этап 6: полный журнал
(defun psp-diag-journal ( / f)
  (setq f (open (strcat (psp-output-dir) "\\spds_diag.txt") "a"))
  (if f
    (progn
      (write-line "---- SPDSFORMAT ----" f)
      (write-line (strcat "ObjectName: "
                          (if (and *psp-d-nm*
                                   (not (vl-catch-all-error-p *psp-d-nm*)))
                            *psp-d-nm* "-")) f)
      (write-line (strcat "candidates: "
                          (if *psp-d-cands* (psp-join ", " *psp-d-cands*) "-")) f)
      (foreach s *psp-d-dictstr*
        (write-line (strcat "DICT: " s)) f)
      (write-line (strcat "texts-in-window: "
                          (itoa (length *psp-d-txts*))) f)
      (write-line (strcat "candidates-from-texts: "
                          (if *psp-d-txtc* (psp-join ", " *psp-d-txtc*) "-")) f)
      (if *psp-d-wr*
        (progn
          (foreach key (car *psp-d-wr*)
            (write-line (strcat "TYPE: " (car key) " | "
                                (itoa (cdr key))) f))
          (foreach key (cadr *psp-d-wr*)
            (write-line (strcat "BLOCK: " (car key) " | "
                                (itoa (cdr key))) f))))
      (write-line (vl-prin1-to-string *psp-d-edd*) f)
      (foreach s *psp-d-strs* (write-line (strcat "STR: " s)) f)
      (foreach s *psp-d-txts* (write-line (strcat "TXT: " s)) f)
      (if *psp-d-wr*
        (foreach s (nth 2 *psp-d-wr*)
          (write-line (strcat "BTXT: " (nth 2 s)) f)))
      (close f)
      (princ (strcat "\nПолные данные дописаны: "
                     (psp-output-dir) "\\spds_diag.txt")))
    (princ "\nФайл журнала недоступен для записи."))
  (princ "\nПришлите весь вывод команды и spds_diag.txt с этой страницы.")
  (princ))
(princ (strcat "\nПСПД: секция 5/6 загружена")) (princ)
;; Диагностика: оркестратор
(defun c:ЭКССПДСДИАГ ()
  (psp-diag-table)
  (if *psp-d-ss*
    (progn
      (psp-diag-extract)
      (psp-diag-dict)
      (psp-diag-texts)
      (psp-diag-window)
      (psp-diag-journal)))
  (princ))

(defun c:SPDSDIAG () (c:ЭКССПДСДИАГ))
(princ (strcat "\nПСПД: секция 6/6 загружена")) (princ)

;; ---------------------------------------------------------------------------
;; Загрузка
;; ---------------------------------------------------------------------------

(princ (strcat "\nЗагружено: PlotSPDSDiag.lsp v" *psp-ver*
               " — диагностика СПДС: ЭКССПДСДИАГ / SPDSDIAG."))
(princ)