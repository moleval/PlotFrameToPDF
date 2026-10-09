;;; -*- coding: windows-1251 -*-
;;; pfp_loader.lsp
;;;
;;; Проверяющий загрузчик модуля PlotFrameToPDF (AutoLispDesign, п.6).
;;;
;;; Загрузка: (load "<папка>/pfp_loader.lsp")
;;; Загрузчик находит PlotFrameToPDF.lsp (по путям поиска или через
;;; диалог), выполняет проверки ДО загрузки (BOM, баланс скобок,
;;; корректность строковых литералов) и только затем загружает модуль
;;; и проверяет доступность команд. При провале проверки модуль
;;; НЕ загружается.

;; Поиск файла модуля: пути поиска AutoCAD, затем диалог выбора.
(defun pfp-load-find-module ( / f)
  (setq f (findfile "PlotFrameToPDF.lsp"))
  (if (null f)
    (setq f (getfiled "Укажите файл PlotFrameToPDF.lsp" "" "lsp" 0)))
  f)

;; Сканирование файла. Состояния: 0=код, 1=строка, 2=комментарий.
;; Возвращает T, если скобки сбалансированы и строки корректны;
;; сообщения об ошибках печатает сама.
(defun pfp-load-scan (path / f c state depth line err)
  (setq state 0 depth 0 line 1 err 0)
  (setq f (open path "r"))
  (while (and f (setq c (read-char f)))
    (cond
      ((= c 10)
       (setq line (1+ line))
       (if (= state 2) (setq state 0)))
      ((= state 2))
      ((= state 1)
       (cond
         ((= c 92) (read-char f))
         ((= c 34) (setq state 0))))
      ((= c 59) (setq state 2))
      ((= c 34) (setq state 1))
      ((= c 40) (setq depth (1+ depth)))
      ((= c 41)
       (setq depth (1- depth))
       (if (< depth 0)
         (progn
           (princ (strcat "\n  | лишняя ')' в строке " (itoa line)))
           (setq err (1+ err))))))
  (if f (close f))
  (if (/= depth 0)
    (progn
      (princ (strcat "\n  | не закрыты скобки: " (itoa depth)))
      (setq err (1+ err))))
  (if (= state 1)
    (progn
      (princ "\n  | есть незакрытая строка")
      (setq err (1+ err))))
  (= err 0))

;; Основная процедура: проверки -> загрузка -> отчёт.
(defun pfp-load-run ( / path f bom ok r res)
  (princ "\nПФП-загрузчик: проверка модуля перед загрузкой.")
  (setq path (pfp-load-find-module))
  (cond
    ((null path)
     (princ "\n  | файл PlotFrameToPDF.lsp не найден — загрузка отменена.")
     (setq ok nil))
    (t
     (princ (strcat "\n  | файл: " path))
     ;; Проверка BOM (первые байты UTF-8: 239 187 191)
     (setq f (open path "r"))
     (setq bom (list (read-char f) (read-char f) (read-char f)))
     (close f)
     (setq ok (not (equal bom (list 239 187 191))))
     (if ok
       nil
       (princ "\n  | файл в UTF-8 с BOM, требуется CP1251 (ANSI) без BOM."))
     ;; Проверка структуры (скобки, строки)
     (if ok
       (progn
         (setq r (pfp-load-scan path))
         (setq ok r)
         (if ok
           (princ "\n  | скобки: сбалансированы; строки: корректны.")
           (princ "\n  | ошибки структуры (см. выше)."))))
     ;; Загрузка модуля
     (if ok
       (progn
         (princ "\nПФП-загрузчик: проверки пройдены, загружаю модуль.")
         (setq res (vl-catch-all-apply 'load (list path)))
         (if (vl-catch-all-error-p res)
           (progn
             (princ (strcat "\n  | ошибка загрузки: "
                            (vl-catch-all-error-message res)))
             (setq ok nil)))))
     ;; Проверка доступности команд
     (if (and ok c:ЭКСВПДФ c:EXPTPDF)
       (princ "\n  | команды: ЭКСВПДФ / EXPTPDF доступны.")
       (if ok
         (progn
           (princ "\n  | команды не определены после загрузки.")
           (setq ok nil))))
     ;; Итоговый отчёт (табличный вид)
     (princ "\n----------------------------------------")
     (princ "\n| Проверка         | Результат          |")
     (princ "\n|------------------|--------------------|")
     (princ (strcat "\n| модуль           | "
                    (if ok "загружен" "НЕ загружен")
                    "          |"))
     (princ "\n----------------------------------------"))))
  (princ))

(pfp-load-run)
(princ)
