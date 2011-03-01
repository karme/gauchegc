;;; $Id: plot-test.scm,v 1.2 2002/08/17 15:30:15 skimu Exp $
;;;
;;; A simple example of plot!
;;;

(use srfi-1)
(use ggc.skimu.frame-buffer) ; drawing
(use ggc.skimu.plot)

(define sinpoints
  (map (lambda (x)
	 (cons x (sin x)))
       (iota 3140 0 0.01)))

(define singraph (plot! sinpoints "X" "Sin(X)"))
;(fb-view singraph)
(save-frame-buffer-as-png-file singraph "sin.png")
