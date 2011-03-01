(define-module ggc.skimu.gnuplot
  (use gauche.process)
  (export
   gplt-select-graphics
   gplt-clear-graphics
   gplt-text
   gplt-reset
   gplt-move
   gplt-vector
   gplt-set-linetype
   gplt-set-default-linetype
   gplt-put-text
   gplt-set-text-justify 
   gplt-set-linewidth 
   gplt-point
   gplt-set-pointsize
   gplt-fillbox
   gplt-draw-line
   gplt-update-linetype
   gplt-view
   gplt-open-graphics
   gplt-open-window
   gplt-clear-window
   gplt-close-graphics
   ))
(select-module ggc.skimu.gnuplot)

(define *gplt-xmax* 4096)
(define *gplt-ymax* 4096)
(define *gplt-max-windows* 16)
(define *point-types* 6)

(define (gplt-select-graphics n)
  (format *gp* "G~01,'0d~%" n))

(define (gplt-clear-graphics n)
  (format *gp* "C~01,'0d~%" n))

(define (gplt-text)
  (format *gp* "E~%")
  (flush  *gp*))

(define (gplt-reset)
  (format *gp* "R~%"))

(define (gplt-move x y)
    (format *gp* "M~4,'0d~4,'0d~%" x y))
;(gplt-move 10 50)

(define (gplt-vector x y)
    (format *gp* "V~4,'0d~4,'0d~%" x y))

(define (gplt-set-linetype lt)
  ; lt    
  ; -2 :  solid black (used in frame)
  ; -1 :  dotted black line
  ;  0 :  red
  ;  1 :  green
  ;  2 :  blue
  ;  3 :
  ;  4 :
  ;  5 :
  ;  6 :
  ;  7 :
  ;  8 -> 0
  ;  9 -> 1
  ;  ...
    (format *gp* "L~4,'0d~%" lt))
; (gplt-linetype -2) -> "L00-2"

(define (gplt-set-default-linetype)
  (format *gp* "L-001~%"))

(define (gplt-put-text x y str)
  (format *gp* "T~4,'0d~4,'0d~a~%" x y str))

(define (gplt-set-text-justify mode)
      ; text justification mode
      ; modes are
      ;   0 LEFT
      ;   1 CENTRE
      ;   2 RIGHT
    (format *gp* "J~4,'0d~%" mode))

(define (gplt-set-linewidth lw)
  (format *gp* "W~4,'0d~%" lw))

(define (gplt-point x y num)
  ; num
  ;     0 dot
  ;     1 diamond
  ;     2 plus
  ;     3 box
  ;     4 X
  ;     5 triangle
  ;     6 star
  ;
  (format *gp* "P~1,'0d~4,'0d~4,'0d~%" num x y))

(define (gplt-set-pointsize x y)
  (format *gp* "P7~4,'0d~4,'0d~%" x y))

(define (gplt-fillbox style x y w h)
  ; style will be ignored
  (format *gp* "F~4,'0d~4,'0d~4,'0d~4,'0d~4,'0d~%" style x y w h))

(define *gp* #f)

(define (already-opened?) *gp*)

(define plot-command (cond 
                      ((file-exists? "/usr/local/bin/comm_plot_glut")
                       "/usr/local/bin/comm_plot_glut")
                      ((file-exists? "/usr/local/bin/plot_glut")
                       "/usr/local/bin/plot_glut")
                      ((file-exists? "/usr/local/bin/gnuplot_x11")
                       "gnuplot_x11 -geometry 400x400")
                      ((file-exists? "/usr/X11R6/bin/gnuplot_x11")
                       "gnuplot_x11 -geometry 400x400")
                      (else
                       (error "Could not find plot helper command\n"))))

(define (gplt-open-graphics . rest)
  (or (already-opened?)
      (if (null? rest)
	  (set! *gp* (open-output-process-port plot-command))
	  (cond ((string? (car rest))
		 (set! *gp* (open-output-file (car rest))))
		((port? (car rest))
		 (set! *gp* (car rest)))
		(else
		 (error "Arguments must be filename or port."))))))

(define (gplt-open-window n)
  (gplt-select-graphics n)
  (gplt-clear-graphics n)
  (gplt-set-linetype 2)
  (gplt-set-linewidth 1)
  (gplt-set-pointsize 40 40)
  (gplt-text))
    
(define (gplt-clear-window n)
  (gplt-select-graphics n)
  (gplt-clear-graphics n)
  (gplt-set-linetype 2)
  (gplt-set-linewidth 1)
  (gplt-text))

(define (gplt-close-graphics)
  (gplt-reset)
  (flush *gp*)
  (close-output-port *gp*)
  (set! *gp* #f)
  'ok)

(define (gplt-view n) 
  (gplt-select-graphics n)
  (gplt-text))

(define gplt-update-linetype
  (let ((lt 0))
    (lambda ()
      (set! lt (+ lt 1))
      (gplt-set-linetype lt))))

(define (gplt-draw-line n x1 y1 x2 y2)
  (let ((ex1 (inexact->exact x1))
	(ey1 (inexact->exact y1))
	(ex2 (inexact->exact x2))
	(ey2 (inexact->exact y2)))
    (gplt-select-graphics n)
    (gplt-move ex1 ey1)
    (gplt-vector ex2 ey2)
    (gplt-text)
    ))
(provide "ggc/skimu/gnuplot")

;;; EOF
