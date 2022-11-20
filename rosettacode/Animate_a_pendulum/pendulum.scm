#!/usr/bin/env gosh
#| -*- mode: scheme; coding: utf-8; -*- |#
(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use math.const)
(define (deg->rad degree) (* (/ degree 180) pi))
(define (rad->deg radians) (* (/ radians pi) 180))
(define (main args)
  (glut-init args)
  (let* ((φ (deg->rad 179)) (l 0.5) (bob 0.02) (q (make <glu-quadric>))
	 (draw-pendulum (lambda()
  			  (gl-push-matrix*
			   (gl-scale 4 4 4)
			   (gl-translate 0 l 0)
			   (gl-rotate (rad->deg φ) 0 0 1)
			   (gl-begin GL_LINES)
			   (gl-vertex 0 0)
			   (gl-vertex 0 (- l))
			   (gl-end)
			   (gl-translate 0 (- l) 0)
			   (glu-sphere q bob 10 10))))
	 (g 9.81)
	 (φ̇ 0)
	 (euler-step (lambda(h)
		       (inc! φ̇ (* (- (* (/ g l) (sin φ))) h))
		       (inc! φ (* φ̇ h)))))
    (simple-viewer-display
     (lambda ()
       ;; I hope sync to VBLANK aka VSYNC works and the display has ~60Hz
       (euler-step 1/60)
       (draw-pendulum)
       (glut-post-redisplay))))
  (simple-viewer-window 'pendulum)
  (glut-full-screen)
  (simple-viewer-run :rescue-errors #f))
