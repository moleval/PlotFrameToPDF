;;; PlotFrameToFP.lsp Ч печать рамки чертежа на системный принтер
;;; (FinePrint и любой другой, заданный в ѕ—ѕЌј—“–ќ… »).
;;; v1.01 Ч системный принтер: свой хвост ленты (без имени файла).
;;;  оманды: Ё —¬ѕ‘ѕј4 / EXPTFPA4 Ч участок чертежа на A4,
;;;          Ё —¬ѕ‘ѕј3 / EXPTFPA3 Ч участок чертежа на A3.
;;; ќриентаци€ распознаЄтс€ по рамке (альбом/портрет). ‘ормат
;;; ‘» —»–ќ¬јЌЌџ… и —“јЌƒј–“Ќџ… (A4/A3, с пол€ми принтера), масштаб
;;; Ђ¬писатьї, по центру. ÷вет и CTB Ч общие с базой и —ѕƒ—
;;; (ѕ—ѕЌј—“–ќ… » -> AppData/PlotFrame). ѕринтеры дл€ A4 и A3 Ч
;;; свои, задаютс€ в ѕ—ѕЌј—“–ќ… », умолчание FinePrint.
;;; ѕечать лентами -ѕ≈„ј“№: формат предустанавливаетс€ через ActiveX
;;; (разрешЄнное исключение) и в ленту не посылаетс€; вопрос Ђ«апи-
;;; сать изменени€ в макет?ї непосто€нен Ч сбой ленты перехватывает-
;;; с€, выполн€етс€ втора€ попытка с ЂЌетї в хвосте. –езультат Ќ≈
;;; открываетс€ и не удал€етс€ Ч FinePrint показывает своЄ окно.

;; v1.01: системный принтер опрашиваетс€ иначе, чем PDF: имени
;; файла Ќ≈“, вместо него Ђ«апись чертежа в файл? <Ќ>ї (ответ Ќ);
;; Ђ—охранить изменени€ параметров листа?ї непосто€нен Ч закрыва-
;; етс€ второй попыткой. fp-cfg: пустое значение = умолчание
;; (getcfg отсутствующего ключа возвращает "" Ч из-за этого бана-
;; нер печатал Ђпринтер: ї без имени).
;; T: слать ЂЌетї на Ђћасштабировать веса линий?ї (не кажда€ сборка
;; задаЄт этот вопрос; переключение Ч ‘ѕЋ≈Ќ“ј / FPTAPE)
(setq *fp-tape-mode* nil)
;; T: не глушить эхо -ѕ≈„ј“№ (диагностика; ‘ѕЁ’ќ¬ Ћ / ‘ѕЁ’ќ¬џ Ћ)
(setq *fp-plot-echo* nil)

;; «начение реестра настроек или умолчание (пуста€ строка Ч тоже
;; Ђнет значени€ї: getcfg отсутствующего ключа даЄт "")
(defun fp-cfg (key def / s)
  (setq s (getcfg key))
  (if (and (= (type s) 'STR) (/= s "")) s def))

(defun fp-s->str (x)
  (cond
    ((= (type x) 'STR) x)
    ((null x) "")
    (t (vl-princ-to-string x))))

;; —брос зависшей -ѕ≈„ј“№ (Ctrl+C через SendCommand, без Enter)
(defun fp-clear-stuck ( / doc i)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (setq i 0)
  (while (and (< i 5) (> (getvar "CMDACTIVE") 0))
    (vl-catch-all-apply 'vla-SendCommand (list doc (chr 27)))
    (setq i (1+ i)))
  (princ))

;; ActiveX-обЄртки (только предустановка/восстановление листа Ч
;; разрешЄнное исключение перед -ѕ≈„ј“№)
(defun fp-get (obj prop)
  (vl-catch-all-apply 'vlax-get-property (list obj prop)))

(defun fp-safe-put (obj prop val)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 'vlax-put-property (list obj prop val)))))

;; ѕредустановка принтера и формата активного листа; прежние
;; значени€ -> *fp-page-saved* -> fp-page-restore
(defun fp-page-preset (printer fmt / doc layout cfg med)
  (setq *fp-page-saved* nil)
  (setq doc (fp-get (vlax-get-acad-object) 'ActiveDocument))
  (setq layout (fp-get doc 'ActiveLayout))
  (if (or (null layout) (vl-catch-all-error-p layout))
    nil
    (progn
      (setq cfg (fp-get layout 'ConfigName)
            med (fp-get layout 'CanonicalMediaName))
      (setq *fp-page-saved*
            (list (if (vl-catch-all-error-p cfg) nil cfg)
                  (if (vl-catch-all-error-p med) nil med)))
      (fp-safe-put layout 'ConfigName printer)
      (fp-safe-put layout 'CanonicalMediaName fmt))))

(defun fp-page-restore ( / doc layout)
  (if *fp-page-saved*
    (progn
      (setq doc (fp-get (vlax-get-acad-object) 'ActiveDocument))
      (setq layout (fp-get doc 'ActiveLayout))
      (if (and (not (null layout)) (not (vl-catch-all-error-p layout)))
        (progn
          (if (car *fp-page-saved*)
            (fp-safe-put layout 'ConfigName (car *fp-page-saved*)))
          (if (cadr *fp-page-saved*)
            (fp-safe-put layout 'CanonicalMediaName
                         (cadr *fp-page-saved*)))))
      (setq *fp-page-saved* nil)))
  (princ))

;; ќсновна€ процедура: рамка -> принтер, формат fmt ("A4"/"A3")
(defun fp-run (fmt / pt1 pt2 x1 y1 x2 y2 w h rot ctb printer
                  old-bg old-paper old-cmdecho attempt tape res fail)
  (setq ctb (if (= (fp-cfg "AppData/PlotFrame/ColorMode" "0") "1")
              (fp-cfg "AppData/PlotFrame/CtbColor" "acad.ctb")
              (fp-cfg "AppData/PlotFrame/CtbMono" "monochrome.ctb")))
  (setq printer (fp-cfg (strcat "AppData/PlotFrame/FPPrinter" fmt)
                        "FinePrint"))
  (setq old-bg      (getvar "BACKGROUNDPLOT"))
  (setq old-paper   (vl-catch-all-apply 'getvar (list "PAPERUPDATE")))
  (setq old-cmdecho (getvar "CMDECHO"))
  (fp-clear-stuck)
  (setvar "BACKGROUNDPLOT" 0)
  (if (not (vl-catch-all-error-p old-paper)) (setvar "PAPERUPDATE" 1))
  (princ (strcat "\nЁкспорт рамки на " fmt " (принтер: " printer
                 "). ”кажите рамку выделени€."))
  (setq pt1 (getpoint "\nѕервый угол рамки: "))
  (cond
    ((null pt1)
     (princ "\nќтменено.")
     (setvar "BACKGROUNDPLOT" old-bg)
     (if (not (vl-catch-all-error-p old-paper))
       (setvar "PAPERUPDATE" old-paper)))
    (t
     (setq pt2 (getcorner pt1 "\n¬торой угол рамки: "))
     (cond
       ((null pt2)
        (princ "\nќтменено.")
        (setvar "BACKGROUNDPLOT" old-bg)
        (if (not (vl-catch-all-error-p old-paper))
          (setvar "PAPERUPDATE" old-paper)))
       (t
        (setq x1 (min (car pt1) (car pt2))
              y1 (min (cadr pt1) (cadr pt2))
              x2 (max (car pt1) (car pt2))
              y2 (max (cadr pt1) (cadr pt2))
              w  (- x2 x1)
              h  (- y2 y1))
        (setq rot (if (>= w h) "_L" "_P"))
        ;; ƒо двух попыток: Ђ«аписать изменени€ в макет?ї задаЄтс€
        ;; не всегда; сбой ленты -> втора€ попытка с ЂЌетї в хвосте
        (setq attempt 0 fail nil)
        (while (< attempt 2)
          (setq attempt (1+ attempt))
          (fp-page-preset printer fmt)
          (setvar "CMDECHO" (if *fp-plot-echo* 1 0))
          (setq tape
            (append
              (list "_.-plot"
                    "_Y"                   ; подробна€ настройка
                    ""                     ; текущий лист
                    printer                ; системный принтер
                    ""                     ; формат уже предустановлен
                    "_M"                   ; мм
                    rot                    ; ориентаци€ по рамке
                    "_N"                   ; не переворачивать
                    "_W"                   ; область: –амка
                    (list x1 y1)           ; нижний левый угол
                    (list x2 y2)           ; верхний правый угол
                    "_F"                   ; вписать
                    "_C"                   ; центрировать
                    "_Y"                   ; учитывать стили печати
                    ctb                    ; CTB (общий с базой/—ѕƒ—)
                    "_Y")                  ; учитывать веса линий
              (if *fp-tape-mode*           ; Ђћасштабировать веса?ї
                (list "_N"))               ; (‘ѕЋ≈Ќ“ј)
              (if (= 1 (getvar "TILEMODE"))
                ;; --- вкладка ћодель (системный принтер: имени файла
                ;; Ќ≈“, вместо него Ђ«апись чертежа в файл?ї -> Ќ)
                (append
                  (list "_As" "_N")        ; тонирование; в файл: нет
                  (if (= attempt 2)        ; Ђ—охранить изменени€ пара-
                    (list "_N") nil)))     ;метров листа?ї задаЄт не
              ;; --- вкладка Ћист ---        ; кажда€ сборка -> 2-€
              (append                      ; попытка
                (list "_N" "_N" "_N")      ; весов/лист первым/скрыть
                (if (= attempt 2)          ; Ђзаписать изменени€ї Ч
                  (list "_N") nil)         ; непосто€нный вопрос
                (list "_N" "_Y"))))        ; в файл: нет; печатать: да
          (setq res (vl-catch-all-apply 'apply (list 'command tape)))
          (setvar "CMDECHO" old-cmdecho)
          (fp-page-restore)
          (cond
            ((vl-catch-all-error-p res)
             (fp-clear-stuck)
             (setq fail T)
             (if (< attempt 2)
               (princ "\n‘ѕ: лента сорвалась Ч втора€ попытка (плюс ЂЌетї на Ђзаписать изменени€ї).")))
            ((> (getvar "CMDACTIVE") 0)
             (fp-clear-stuck)
             (setq fail T)
             (if (< attempt 2)
               (princ "\n‘ѕ: -ѕ≈„ј“№ ждЄт ответа Ч втора€ попытка (плюс ЂЌетї на Ђзаписать изменени€ї).")))
            (t (setq fail nil attempt 2))))
        (if fail
          (princ "\n‘ѕ: отправить не удалось Ч включите ‘ѕЁ’ќ¬ Ћ и повторите.")
          (princ (strcat "\n‘ѕ: отправлено на печать (" fmt
                         " -> " printer ").")))
        (setvar "BACKGROUNDPLOT" old-bg)
        (if (not (vl-catch-all-error-p old-paper))
          (setvar "PAPERUPDATE" old-paper))))))
  (princ))

;; Ёхо ленты -ѕ≈„ј“№ (диагностика)
(defun fp-echo-set (v)
  (setq *fp-plot-echo* v)
  (princ (if v
    "\n‘ѕ: эхо печати ¬ Ћё„≈Ќќ Ч подсказки -ѕ≈„ј“№ будут видны."
    "\n‘ѕ: эхо печати выключено."))
  (princ))
(defun c:‘ѕЁ’ќ¬ Ћ () (fp-echo-set T))
(defun c:‘ѕЁ’ќ¬џ Ћ () (fp-echo-set nil))
(defun c:FPECHOON () (c:‘ѕЁ’ќ¬ Ћ))
(defun c:FPECHOOFF () (c:‘ѕЁ’ќ¬џ Ћ))

;; ѕереключение ленты -ѕ≈„ј“№: с ответом на Ђћасштабировать веса?ї
(defun c:‘ѕЋ≈Ќ“ј ( )
  (setq *fp-tape-mode* (not *fp-tape-mode*))
  (princ (if *fp-tape-mode*
    "\n‘ѕ: в ленту добавл€етс€ ЂЌетї на Ђћасштабировать веса линийї."
    "\n‘ѕ: Ђћасштабировать веса линийї не отвечаетс€."))
  (princ))
(defun c:FPTAPE () (c:‘ѕЋ≈Ќ“ј))

;;  оманды
(defun c:Ё —¬ѕ‘ѕј4 () (fp-run "A4"))
(defun c:Ё —¬ѕ‘ѕј3 () (fp-run "A3"))
(defun c:EXPTFPA4 () (c:Ё —¬ѕ‘ѕј4))
(defun c:EXPTFPA3 () (c:Ё —¬ѕ‘ѕј3))

(princ "\n«агружено: PlotFrameToFP.lsp v1.00 Ч команды: Ё —¬ѕ‘ѕј4 / EXPTFPA4 (A4), Ё —¬ѕ‘ѕј3 / EXPTFPA3 (A3).")
(princ "\n‘ѕ: принтеры дл€ A4 и A3 задаютс€ в ѕ—ѕЌј—“–ќ… » (умолчание FinePrint).")
(princ)
