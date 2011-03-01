(define-module ggc.skimu.plot
  (use ggc.skimu.frame-buffer)
  (export plot!)
)

(select-module ggc.skimu.plot)

(define plot-width 640)
(define plot-height 480)
(define plot-x-margin 40)
(define plot-y-margin 40)
(define make-point cons)
(define point-x car)
(define point-y cdr)

(define (make-transformer xmin xmax ymin ymax)
  (let ((dw (/ (- plot-width  (* 2 plot-x-margin)) (- xmax xmin)))
	(dh (/ (- plot-height (* 2 plot-y-margin)) (- ymax ymin))))
    (lambda (point)
      (let ((x (point-x point))
	    (y (point-y point)))
	(make-point
	 (inexact->exact (+ plot-x-margin (* dw (- x xmin))))
	 (inexact->exact (- plot-height plot-y-margin (* dh (- y ymin)))))))))

(define (find-draw-area pts)
  (let ((fx (point-x (car pts)))
	(fy (point-y (car pts))))
    (let loop ((ps (cdr pts))
	       (xmin fx)(xmax fx)
	       (ymin fy)(ymax fy))
      (if (null? ps)
	  (values xmin xmax ymin ymax)
	  (let ((px (point-x (car ps)))
		(py (point-y (car ps))))
	    (if (< px xmin) (set! xmin px))
	    (if (> px xmax) (set! xmax px))
	    (if (< py ymin) (set! ymin py))
	    (if (> py ymax) (set! ymax py))
	    (loop (cdr ps) xmin xmax ymin ymax))))))

(define fb (make-frame-buffer plot-width plot-height))

(define (draw-frame)
  (let ((x-margin plot-x-margin)
	(y-margin plot-y-margin))
    (let ((x1 x-margin)
	  (y1 y-margin)
	  (x2 (- plot-width x-margin))
	  (y2 (- plot-height y-margin)))
      (fb-draw-box! fb x1 y1 x2 y2 0 0 0))))

(define (plot-points trans points)
  (fb-draw-points! fb
		   (map (lambda (point)
			  (trans point)) points)))

(define (plot! points xlabel ylabel)
  (let ((trans (receive (xmin xmax ymin ymax) (find-draw-area points)
		  (make-transformer xmin xmax ymin ymax))))
    (draw-frame)
    (plot-points trans points)
    fb))

(provide "ggc/skimu/plot")
;; EOF
