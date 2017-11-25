#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.process)
(use srfi-19)

(define (time-difference-s t2 t1)
  (time->seconds (time-difference t2 t1)))

(define (ping size)
  (let ((s (make-string size #\x))
	(start (current-time 'time-monotonic)))
    (print s)
    (with-output-to-port (current-error-port)
      (lambda()
	(when (not (string=? (read-line) s))
	  (error "expected" s))
	(print (time-difference-s (current-time 'time-monotonic) start))))))

(define (main args)
  ;; (print (cadr args))
  (let1 p (run-process '(cat) :host (cadr args) :input :pipe :output :pipe)
    (with-ports (process-output p) (process-input p) #f
		(lambda()
		  (dotimes (i 5)
		    (ping 100))))
    (close-port (process-input p))
    (process-wait p #f #t))
  0)
