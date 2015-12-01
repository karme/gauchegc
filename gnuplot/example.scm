;;;
;;;  parallel execution of SICP picuture language
;;;

(use gauche.process)
(use ggc.skimu.gnuplot)
(use ggc.lwp)

(use math.const)  ; definition of pi is required in pict-core.scm


(add-load-path ".")
(load "pict-core.scm")

;;;
;;; interaction or batch
;;;
(define *interactive* #t)

(define (pict-open-gp file)
  (if *interactive*
      (gplt-open-graphics)
      (gplt-open-graphics file)))

(define (pict-close-gp)
  (gplt-close-graphics))

(define (pict-pause)
  (if *interactive* 
      (begin 
        (format #t "--Hit return to continue--")
        (flush (current-output-port))
        (read-byte))))

(define (pict-sleep n)
  (if *interactive* (sys-sleep n)))

(define (pict-mess mess)
  (if *interactive*
      (begin
        (display mess)
        (newline))))

(pict-mess "Welcome to parallel picture drawing demo. We will show you two demos.
Please open Gplt.app (mac) or run ./plot_glut (others) in background.")
(pict-pause)

;;;
;;; drawining
;;;
(define (draw-line w p1 p2)
  (let* ((x1 (xcor-vect p1))
	 (y1 (ycor-vect p1))
	 (x2 (xcor-vect p2))
	 (y2 (ycor-vect p2)))
    (lwp-pause)
    (gplt-draw-line w
		    (round->exact x1)
		    (round->exact y1)
		    (round->exact x2)
		    (round->exact y2))))

(define (apply-painter win painter width height)
  (let ((frame  (make-frame win (make-vect 0.0 0.0)
			    (make-vect width 0.0)
			    (make-vect 0.0 height))))
    (painter frame)))

;;;
;;; painters
;;;
(define clw (cross-limit (compose-painter border wave) 3))
(define csx (corner-split wave 3))
(define sqw (square-limit x-mark 4))

;;;
;;;  DEMO1: draw three windows in parallel
;;;
(pict-mess "The first demo is to draw three windows in parallel.")
(pict-pause)

(pict-open-gp "three-windows-demo.gp")
(gplt-open-window 1)
(gplt-open-window 2)
(gplt-open-window 3)
(lwp (lambda () (apply-painter 1 clw 4096 4096) ))
(lwp (lambda () (apply-painter 2 csx 4096 4096) ))
(lwp (lambda () (apply-painter 3 sqw 4096 4096) ))
(lwp-start)
(gplt-view 3)
(gplt-view 2)
(gplt-view 1)
(pict-pause)
(gplt-clear-window 1)
(gplt-clear-window 2)
(gplt-clear-window 3)
(pict-sleep 1) ;;; wait for the window clear
(pict-close-gp)

;;;
;;; DEMO2: parallel painter
;;;

(pict-mess "The second demo is a parallel verion of painter,
but we will show you normal version first.")
(pict-pause)

(pict-open-gp "clw2-series2.gp")
(gplt-open-window 1)
(apply-painter 1 clw 4096 4096)
(lwp-start)
(gplt-view 1)
(pict-close-gp)

;;;
;;;  parallel version BESIDE and BELOW
;;;
  (define (beside painter1 painter2)
    (let ((split-point (make-vect 0.5 0.0)))
      (let ((paint-left
             (transform-painter painter1
                                (make-vect 0.0 0.0)
                                split-point
                                (make-vect 0.0 1.0)))
            (paint-right
             (transform-painter painter2
                                split-point
                                (make-vect 1.0 0.0)
                                (make-vect 0.5 1.0))))
        (lambda (frame)
          (lwp (lambda () (paint-left  frame) ))
          (lwp (lambda () (paint-right frame) ))
  	))))

  (define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
      (let ((paint-below
             (transform-painter painter1
                                (make-vect 0.0 0.0)
                                (make-vect 1.0 0.0)
                                split-point))
            (paint-above
             (transform-painter painter2
                                split-point
                                (make-vect 1.0 0.5)
                                (make-vect 0.0 1.0))))
        (lambda (frame)
	  (lwp (lambda () (paint-below frame) ))
	  (lwp (lambda () (paint-above frame) ))
  	))))

;;;
;;;
;;;

; redefine clw to make parallel version BESIDE and BELOW effective.
(define clw (cross-limit (compose-painter border wave) 3))

;;;
;;; show parallel version paiter into window 2
;;;

(pict-mess "Are you ready for parallel verion?")
(pict-pause)

(pict-open-gp "clw2-parallel2.gp")
(gplt-open-window 2)
(apply-painter 2 clw 4096 4096)
(lwp-start)
(gplt-view 2)
(pict-pause)
(gplt-clear-window 1)
(gplt-clear-window 2)
(pict-sleep 1)
(pict-close-gp)

(pict-mess "That's it.  Thank you very much.")
;;; EOF
