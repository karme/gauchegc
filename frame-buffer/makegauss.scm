#!/usr/bin/env gosh
;; -*-Scheme-*-
(use ggc.skimu.frame-buffer)

(define (draw-gauss fb)
  (letrec ((W (fb 'width))
	   (H (fb 'height))
	   (W/2 (/ W 2))
	   (H/2 (/ H 2)))
    (do ((i 0 (+ i 1)))
	((= i W))
      (do ((j 0 (+ j 1)))
	  ((= j H))
	(letrec ((x (/ (- i W/2) W))
		 (y (/ (- j H/2) H))
              ;# (val (round->exact (* 255 (exp (- 0 (* x x) (* y y))))))
		 (val (round->exact (abs (* 255 (cos (* 30 (- 0 (* x x) (* y y))))))))
		 )
	  (fb 'set! i j val val val))))))

(define (main args)
  (with-error-handler
   (lambda (e)
     (format (current-error-port) "Error: ~a~%" 
	     (slot-ref e 'message))
     (exit 2))
   (lambda ()
     (if (= (length args) 4)
	 (let ((fb (make-frame-buffer (string->number (list-ref args 1))
				      (string->number (list-ref args 2)))))
	   (draw-gauss fb)
	   (save-frame-buffer-as-jpg-file fb (list-ref args 3))
	   0)
	 (errorf "Usage: ~a WIDTH HIGHT out.jpg~%"
		 (list-ref args 0))))))

;;; EOF
