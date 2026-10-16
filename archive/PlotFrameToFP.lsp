;;; PlotFrameToFP.lsp — печать рамки чертежа на системный принтер
;;; (FinePrint и любой другой, заданный в ПСПНАСТРОЙКИ).
;;; v1.02 — ленты Модель/Лист сняты с эхо-логов (Enter у «Сохранить»).
;;; Команды: ЭКСВПФПА4 / EXPTFPA4 — участок чертежа на A4,
;;;          ЭКСВПФПА3 / EXPTFPA3 — участок чертежа на A3.
;;; Ориентация распознаётся по рамке (альбом/портрет). Формат
;;; ФИКСИРОВАННЫЙ и СТАНДАРТНЫЙ (A4/A3, с полями принтера), масштаб
;;; «Вписать», по центру. Цвет и CTB — общие с базой и СПДС
;;; (ПСПНАСТРОЙКИ -> AppData/PlotFrame). Принтеры для A4 и A3 —
;;; свои, задаются в ПСПНАСТРОЙКИ, умолчание FinePrint.
;;; Печать лентами -ПЕЧАТЬ: формат предустанавливается через ActiveX
;;; (разрешённое исключение) и в ленту не посылается; вопрос «Запи-
;;; сать изменения в макет?» непостоянен — сбой ленты перехватывает-
;;; ся, выполняется вторая попытка с «Нет» в хвосте. Результат НЕ
;;; открывается и не удаляется — FinePrint показывает своё окно.

;; v1.03: баннер печатает фактическую версию файла (в 1.01/1.02
;;        строку баннера забыл обновить — печаталось «v1.00»).
;; v1.02: хвосты лент приведены к фактическим (эхо-логи 1.01):
;;        Модель: _As -> в-файл _N -> сохранить (Enter=Н) -> перей-
;;        ти _Y; Лист: веса _N -> первыми _N -> скрыть _N -> в-файл
;;        _N -> сохранить (Enter) -> перейти _Y. Enter у «Сохранить»
;;        страхует отсутствие вопроса. «Масштабировать веса» на Лис-
;;        те — всегда Н (ФПЛЕНТА больше не влияет).
;; v1.01: системный принтер опрашивается иначе, чем PDF: имени
;; файла НЕТ, вместо него «Запись чертежа в файл? <Н>» (ответ Н);
;; «Сохранить изменения параметров листа?» непостоянен — закрыва-
;; ется второй попыткой. fp-cfg: пустое значение = умолчание
;; (getcfg отсутствующего ключа возвращает "" — из-за этого бана-
;; нер печатал «принтер: » без имени).
;; T: слать «Нет» на «Масштабировать веса линий?» (не каждая сборка
;; задаёт этот вопрос; переключение — ФПЛЕНТА / FPTAPE)
(setq *fp-tape-mode* nil)
;; T: не глушить эхо -ПЕЧАТЬ (диагностика; ФПЭХОВКЛ / ФПЭХОВЫКЛ)
(setq *fp-plot-echo* nil)

;; Значение реестра настроек или умолчание (пустая строка — тоже
;; «нет значения»: getcfg отсутствующего ключа даёт "")
(defun fp-cfg (key def / s)
  (setq s (getcfg key))
  (if (and (= (type s) 'STR) (/= s "")) s def))

(defun fp-s->str (x)
  (cond
    ((= (type x) 'STR) x)
    ((null x) "")
    (t (vl-princ-to-string x))))

;; Сброс зависшей -ПЕЧАТЬ (Ctrl+C через SendCommand, без Enter)
(defun fp-clear-stuck ( / doc i)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (setq i 0)
  (while (and (< i 5) (> (getvar "CMDACTIVE") 0))
    (vl-catch-all-apply 'vla-SendCommand (list doc (chr 27)))
    (setq i (1+ i)))
  (princ))

;; ActiveX-обёртки (только предустановка/восстановление листа —
;; разрешённое исключение перед -ПЕЧАТЬ)
(defun fp-get (obj prop)
  (vl-catch-all-apply 'vlax-get-property (list obj prop)))

(defun fp-safe-put (obj prop val)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 'vlax-put-property (list obj prop val)))))

;; Предустановка принтера и формата активного листа; прежние
;; значения -> *fp-page-saved* -> fp-page-restore
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

;; Основная процедура: рамка -> принтер, формат fmt ("A4"/"A3")
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
  (princ (strcat "\nЭкспорт рамки на " fmt " (принтер: " printer
                 "). Укажите рамку выделения."))
  (setq pt1 (getpoint "\nПервый угол рамки: "))
  (cond
    ((null pt1)
     (princ "\nОтменено.")
     (setvar "BACKGROUNDPLOT" old-bg)
     (if (not (vl-catch-all-error-p old-paper))
       (setvar "PAPERUPDATE" old-paper)))
    (t
     (setq pt2 (getcorner pt1 "\nВторой угол рамки: "))
     (cond
       ((null pt2)
        (princ "\nОтменено.")
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
        ;; До двух попыток: «Записать изменения в макет?» задаётся
        ;; не всегда; сбой ленты -> вторая попытка с «Нет» в хвосте
        (setq attempt 0 fail nil)
        (while (< attempt 2)
          (setq attempt (1+ attempt))
          (fp-page-preset printer fmt)
          (setvar "CMDECHO" (if *fp-plot-echo* 1 0))
          (setq tape
            (append
              (list "_.-plot"
                    "_Y"                   ; подробная настройка
                    ""                     ; текущий лист
                    printer                ; системный принтер
                    ""                     ; формат уже предустановлен
                    "_M"                   ; мм
                    rot                    ; ориентация по рамке
                    "_N"                   ; не переворачивать
                    "_W"                   ; область: Рамка
                    (list x1 y1)           ; нижний левый угол
                    (list x2 y2)           ; верхний правый угол
                    "_F"                   ; вписать
                    "_C"                   ; центрировать
                    "_Y"                   ; учитывать стили печати
                    ctb                    ; CTB (общий с базой/СПДС)
                    "_Y")                  ; учитывать веса линий
              ;; Ленты сняты с эхо-логов 1.01 (Модель и Лист).
              ;; Модель: тонирование (_As) -> «Запись чертежа в файл?»
              ;; (_N) -> «Сохранить изменения параметров листа?»
              ;; (Enter = Н) -> «Перейти к печати?» (_Y = Д). Enter
              ;; вместо Н у «Сохранить» делает ленту устойчивой: если
              ;; вопроса нет, Enter уходит в «Перейти» и даёт Д.
              (if (= 1 (getvar "TILEMODE"))
                (list "_As" "_N" "" "_Y")
                ;; Лист: «Масштабировать веса?» (Н, спрашивает всегда)
                ;; -> «Печатать объекты листа первыми?» (Н) -> «Скрывать
                ;; объекты листа?» (Н) -> «Запись чертежа в файл?» (Н)
                ;; -> «Сохранить изменения» (Enter) -> «Перейти» (_Y)
                (list "_N" "_N" "_N" "_N" "" "_Y"))))
          (setq res (vl-catch-all-apply 'apply (list 'command tape)))
          (setvar "CMDECHO" old-cmdecho)
          (fp-page-restore)
          (cond
            ((vl-catch-all-error-p res)
             (fp-clear-stuck)
             (setq fail T)
             (if (< attempt 2)
               (princ "\nФП: лента сорвалась — вторая попытка.")))
            ((> (getvar "CMDACTIVE") 0)
             (fp-clear-stuck)
             (setq fail T)
             (if (< attempt 2)
               (princ "\nФП: -ПЕЧАТЬ ждёт ответа — вторая попытка.")))
            (t (setq fail nil attempt 2))))
        (if fail
          (princ "\nФП: отправить не удалось — включите ФПЭХОВКЛ и повторите.")
          (princ (strcat "\nФП: отправлено на печать (" fmt
                         " -> " printer ").")))
        (setvar "BACKGROUNDPLOT" old-bg)
        (if (not (vl-catch-all-error-p old-paper))
          (setvar "PAPERUPDATE" old-paper))))))
  (princ))

;; Эхо ленты -ПЕЧАТЬ (диагностика)
(defun fp-echo-set (v)
  (setq *fp-plot-echo* v)
  (princ (if v
    "\nФП: эхо печати ВКЛЮЧЕНО — подсказки -ПЕЧАТЬ будут видны."
    "\nФП: эхо печати выключено."))
  (princ))
(defun c:ФПЭХОВКЛ () (fp-echo-set T))
(defun c:ФПЭХОВЫКЛ () (fp-echo-set nil))
(defun c:FPECHOON () (c:ФПЭХОВКЛ))
(defun c:FPECHOOFF () (c:ФПЭХОВЫКЛ))

;; ФПЛЕНТА: для системного принтера переключение не влияет —
;; «Масштабировать веса?» на Листе отвечается всегда (Н)
(defun c:ФПЛЕНТА ( )
  (princ "\nФП: для системного принтера «Масштабировать веса?» на Листе")
  (princ "\n  отвечается всегда (_N) — переключение не требуется.")
  (princ))
(defun c:FPTAPE () (c:ФПЛЕНТА))

;; Команды
(defun c:ЭКСВПФПА4 () (fp-run "A4"))
(defun c:ЭКСВПФПА3 () (fp-run "A3"))
(defun c:EXPTFPA4 () (c:ЭКСВПФПА4))
(defun c:EXPTFPA3 () (c:ЭКСВПФПА3))

(princ "\nЗагружено: PlotFrameToFP.lsp v1.03 — команды: ЭКСВПФПА4 / EXPTFPA4 (A4), ЭКСВПФПА3 / EXPTFPA3 (A3).")
(princ "\nФП: принтеры для A4 и A3 задаются в ПСПНАСТРОЙКИ (умолчание FinePrint).")
(princ)
