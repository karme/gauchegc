;;;
;;; 
;;;

(define-module ggc.numerical.extra
  (export 
          %erf %erfc %grand  ; defined in c-code.
	  erf erfc grand     ; defined in this file
          sci eng engu
  )
  (dynamic-load "ggcnumextra")
 )

(select-module ggc.numerical.extra)

(define erf %erf)
(define erfc %erfc)
(define grand %grand)

(define (format-float x n s)
  (letrec ((o (inexact->exact (floor (/ (log (abs x))
                                        (log 10)))))
           (e (inexact->exact (* s 1.0 (floor (/ o s)))))
           (a (inexact->exact (round (* x (expt 10 (- n o)))))))
    (values (exact->inexact (/ a (expt 10 (+ n (- e o)))))
            e)))

(define (sci x n)
  (receive (a e) (format-float x (- n 1) 1)
    (string-append (if (< a 0) "-" " ")
                   (number->string (abs a))
                   (if (< e 0) "e-" "e+")
                   (number->string (abs e)))))

(define (eng x n)
  (receive (a e) (format-float x (- n 1) 3)
    (string-append (if (< a 0) "-" " ")
                   (number->string (abs a))
                   (if (< e 0) "e-" "e+")
                   (number->string (abs e)))))

(define (engu x n)
  (receive (a e) (format-float x (- n 1) 3)
    (string-append (if (< a 0) "-" " ")
                   (number->string (abs a))
                   (case e
                     ((-18) "a")
                     ((-15) "f")
                     ((-12) "p")
                     ((-9)  "n")
                     ((-6)  "u")
                     ((-3)  "m")
                     ((0)   "")
                     ((3)   "K")
                     ((6)   "M")
                     ((9)   "G")
                     ((12)  "T")
                     (else  "Too large or too small")))))

(provide "ggc/numerical/extra")

;;; EOF
