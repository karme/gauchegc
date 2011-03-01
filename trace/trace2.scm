; Gauche Garbage Collection, ggc.debug.trace.
; written by skimu. See ggc/COPYING for detail.
;
; trace.scm : Macro for tracing procedure call.
;
; Example:
; 
; (define (tak x y z)
;   (if (<= x y)
;       z
;       (tak (tak (- x 1) y z)
; 	     (tak (- y 1) z x)
; 	     (tak (- z 1) x y))))
;
; (define (factorial n)
;   (if (= n 1)
;       1
;       (* n (factorial (- n 1)))))
;
; gosh> (use ggc.debug.trace)
; gosh> (trace factorial)
; gosh> (factorial 5)
; 0:(factorial 5)
; 1:  (factorial 4)
; 2:    (factorial 3)
; 3:      (factorial 2)
; 4:        (factorial 1)
;           ->1
;         ->2
;       ->6
;     ->24
;   ->120
; trace: factorial has been called 5 times.
; 120
; gosh>
; try,
; (trace tak)
; (tak 6 4 2)
; --------------------------------------------------------

(define-module ggc.debug.trace
  (export trace)
)

(select-module ggc.debug.trace)

(define-macro (trace proc)
  `(let ((proc-under-trace ,proc))
     (set! ,proc 
	   (let ((level 0)
		 (count 0)
		 (retval #f))
	     (lambda x 
	       (format #t "~a:~a~a~%" 
		       level 
		       (make-string (* 2 level) #\space) 
		       (cons ',proc x))

	       (set! level (+ level 1))
	       (set! count (+ count 1))
	       (set! retval (apply proc-under-trace x))

	       (format #t "~a->~a~%" 
		       (make-string (* 2 level) #\space) 
		       retval)
	       (set! level (- level 1))
	       (when (= level 0)
		   (format #t "trace: ~a has been called ~a times.~%"
			   ',proc count)
		   (set! ,proc proc-under-trace))
	       retval)))))

;(define procedure-under-trace '())
; (define-macro (untrace proc)
;   `(begin
;      (if (null? procedure-under-trace)
; 	 (error "No procedure is under trace"))
;      (set! ,proc (cdr procedure-under-trace))
;      (set! procedure-under-trace '())))

(provide "ggc/debug/trace")
;; EOF
