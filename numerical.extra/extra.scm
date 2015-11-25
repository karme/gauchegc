;;;
;;; 
;;;
(define-module ggc.numerical.extra
  (export 
          %erf %erfc %grand  ; defined in c-code.
	  erf erfc grand     ; defined in this file
  )
  (dynamic-load "ggcnumextra")
 )

(select-module ggc.numerical.extra)

(define erf %erf)
(define erfc %erfc)
(define grand %grand)

(provide "ggc/numerical/extra")
;;; EOF
