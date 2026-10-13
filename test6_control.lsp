(defun c:ПСППАПКА ( / d)
  (setq d (psp-output-dir))
  (if (vl-file-directory-p d)
    (progn
      (startapp "explorer.exe" d)
      (princ (strcat "\nПСП: папка открыта — " d)))
    (princ (strcat "\nПСП: папка не существует — " d)))
  (princ))
(defun c:PSPPAPKA () (c:ПСППАПКА))
(defun psp-echo-set (v)
  (setq *psp-plot-echo* v)
  (princ (if v
    "\nПСП: эхо печати ВКЛЮЧЕНО — подсказки -ПЕЧАТЬ будут видны."
    "\nПСП: эхо печати выключено."))
  (princ))
(defun c:ПСПЭХОВКЛ () (psp-echo-set T))
(defun c:ПСПЭХОВЫКЛ () (psp-echo-set nil))
(defun c:PSPECHOON () (psp-echo-set T))
(defun c:PSPECHOOFF () (psp-echo-set nil))
(defun c:ПСПЛЕНТА ( )
  (setq *psp-tape-mode* (not *psp-tape-mode*))
  (princ (if *psp-tape-mode*
    "\nПСП: лента с ответом «Масштабировать веса линий?»."
    "\nПСП: лента без ответа «Масштабировать веса линий?» (как в транскрипте 0.24)."))
  (princ))
(defun c:PSPTAPE () (c:ПСПЛЕНТА))
(defun c:ПСПГС ( / p)
  (setq p (getstring T
    "\nПолный путь к gswin64c.exe (Enter — очистить): "))
  (if (= p "")
    (progn
      (setq *psp-gs* nil)
      (setcfg "AppData/PlotSPDS/GS" "")
      (princ "\nПСП: путь к Ghostscript очищен, поиск автоматический."))
    (progn
      (setq p (vl-string-trim " \"" p))
      (setq *psp-gs* p)
      (setcfg "AppData/PlotSPDS/GS" p)
      (princ (strcat "\nПСП: Ghostscript: " p
                     "\nПСП: если склейка не соберётся — проверьте путь."))))
  (princ))
(defun c:PSPGS () (c:ПСПГС))
(princ "\nT6-OK")(princ)
