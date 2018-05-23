#lang cbpv

(bind x
  (beep true)
  (bind y
        (beep false)
        (return true))
  (return y))
(beep x)
(define x false)
(beep x)
(beep x)

(return true)

;; (main
;;  (beep true
;;        (beep false
;;              (return true))))

;; (define x true)
;; (bind y
;;   (beep true)
;;   (return true)
;;   )
;; (beep true)
;; (beep false)
;; (return true)
