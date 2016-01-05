;;;
;;;  Example for gosh-pseduo-session
;;;

;;;
;;; SEC1: hello, world
;;;

;; In Gauche, ``define'' returns symbol which has been defined.

(define (hello)
  (display "hello, world")
  (newline))

;; return value of function ``hello'' is the value returned
;; by ``newline'' which is always #<undef>.

(hello)

;;
;; SEC2: call/cc as goto, example... guess what's gonna happen.
;;

((call/cc
  (lambda (goto)
    (letrec ((start (lambda ()
                      (print "start")
                      (goto next)))
             (third (lambda ()
                      (print "third")
                      (goto last)))
             (next  (lambda ()
                      (print "next")
                      (goto third)))
             (last  (lambda ()
                      (print "last")
                      (+ 3 4))))
      start))))

;; EOF
