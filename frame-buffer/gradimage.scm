#!/usr/bin/env gosh
;; -*-Scheme-*-

;;; for in-place test
;(add-load-path ".") (load "frame-buffer.scm")
;;;

(use gauche.uvector)
(use ggc.numerical.fftn)
(use ggc.skimu.frame-buffer)

(define color-name '("RED" "GREEN" "BLUE"))

(define (gradient fb)
  (letrec ((W      (fb 'width))
	   (H      (fb 'height))
	   (N      (* W H))
	   (img   (fb 'img))
	   (buffr (make-f64vector  N 0.0))
	   (buffc (make-f64vector  N 0.0))
	   (buffr1 (make-f64vector  N 0.0))
	   (buffc1 (make-f64vector  N 0.0))
	   (buffr2 (make-f64vector  N 0.0))
	   (buffc2 (make-f64vector  N 0.0))
	   )
    (display "W=")(display W)(display " H=")(display H)(newline)
    (do ((c 0 (+ c 1)))
	((= c 3))
      (display "---")
      (display (list-ref color-name c))(newline)
      (display "elements copy")(newline)
      (do ((i 0 (+ i 1))
	   (j c (+ j 3)))
	  ((= i N))
	(f64vector-set! buffr i (exact->inexact (u8vector-ref img j)))
	(f64vector-set! buffc i 0.0))
      (fftn (list W H) buffr buffc 1 -2.0)

      (display "diffrentiation")(newline)
      (do ((ky 0 (+ ky 1)))
	  ((= ky H))
	(do ((kx 0 (+ kx 1)))
	    ((= kx W))
	  (let ((off (+ (* W ky) kx))
;		(passband (< rr (+ (* kx kx) (* ky ky))))
		(passband #t)
		)
	    (f64vector-set! buffr1 off
			    (if passband
				(* kx (f64vector-ref buffr off))
				0.0))
	    (f64vector-set! buffc1 off
			    (if passband
				(* kx (f64vector-ref buffc off))
				0.0))
	    (f64vector-set! buffr2 off
			    (if passband
				(* ky (f64vector-ref buffr off))
				0.0))

	    (f64vector-set! buffc2 off
			    (if passband
				(* ky (f64vector-ref buffc off))
				0.0))
	    )))
      (fftn (list W H) buffr1 buffc1 -1 -2.0)
      (fftn (list W H) buffr2 buffc2 -1 -2.0)

      (display "magnitude") (newline)
      (let ((maxval 0.0))
	(do ((ky 0 (+ ky 1)))
	    ((= ky H))
	  (do ((kx 0 (+ kx 1)))
	      ((= kx W))
	    (let ((off (+ (* W ky) kx)))
	      (let ((absgrad (+ (expt (f64vector-ref buffr1 off) 2)
				(expt (f64vector-ref buffr2 off) 2))))
		(if (> absgrad maxval) (set! maxval absgrad))
		(f64vector-set! buffr off absgrad)))))
	(set! maxval (/ 255.0 maxval))
	(display "Maxval=")(display maxval)(newline)
	
	;;------------------------------------
	(display "Normalize...")(newline)
	(do ((ky 0 (+ ky 1)))
	    ((= ky H))
	  (do ((kx 0 (+ kx 1)))
	      ((= kx W))
	    (let ((off (+ (* W ky) kx)))
	      (f64vector-set! buffr off 
			      (* maxval (f64vector-ref buffr off)))))))
      (do ((i 0 (+ i 1))
	   (j c (+ j 3)))
	  ((= i N))
	(u8vector-set! img j (inexact->exact (f64vector-ref buffr i))))
      )))


(define (main args)
  (with-error-handler
   (lambda (e)
     (format (current-error-port) "Error: ~a~%" 
	     (slot-ref e 'message))
     (exit 2))
   (lambda ()
     (if (= (length args) 3)
	 (let ((fb (load-frame-buffer-from-jpg-file (list-ref args 1))))
	   (gradient fb)
	   (save-frame-buffer-as-jpg-file fb (list-ref args 2))
	   0)
	 (errorf "Usage: ~a in.jpg out.jpg~%"
		 (list-ref args 0))))))

;; EOF
