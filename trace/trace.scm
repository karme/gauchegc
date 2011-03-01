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
  (export trace
	  *procedure-under-trace*
	  untrace
	  )
)

(select-module ggc.debug.trace)

(define *procedure-under-trace* '())

(define-macro (trace proc)
  (let ((proc-under-trace (gensym))
        (level (gensym))
        (count (gensym))
        (retval (gensym)))
    `(let ((,proc-under-trace ,proc))
       (if (not (null? *procedure-under-trace*))
           (error "Another procedure is in the way, (untrace) first."))
       (set! *procedure-under-trace* (cons ',proc ,proc-under-trace))
       (set! ,proc 
             (let ((,level 0)
                   (,count 0)
                   (,retval #f))
               (lambda x 
                 (format #t "~a:~a~,,,,50:a~%" 
;                 (format #t "~a:~a~a~%" 
                         ,level 
                         (make-string (* 2 ,level) #\space) 
                         (cons ',proc x))
                 
                 (set! ,level (+ ,level 1))
                 (set! ,count (+ ,count 1))
                 (set! ,retval (apply ,proc-under-trace x))
                 
                 (format #t "~a->~a~%" 
                         (make-string (* 2 ,level) #\space) 
                         ,retval)
                 (set! ,level (- ,level 1))
                 (when (= ,level 0)
                       (format #t "; trace: ~a has been called ~a times.~%"
                               ',proc ,count))
                 ,retval))))))

 (define-macro (untrace)
   `(begin
      (when (not (null? *procedure-under-trace*))
	    (set! ,(car *procedure-under-trace*) 
                  ,(cdr *procedure-under-trace*))
	    (set! *procedure-under-trace* '()))))

;(trace factorial)
;(factorial 5)
;(%macro-expand (trace factorial))
;(%macro-expand (untrace))
(provide "ggc/debug/trace")
;; EOF
