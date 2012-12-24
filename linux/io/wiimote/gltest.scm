#!/bin/bash -xe
#| -*- mode: scheme; coding: utf-8; -*- |#
#|
exec gosh -I. -I../sequencer "$0" "$@"
|#

;;;
;;; simple test using wiimote to make some "music"
;;;

(use gl)
(use gl.glut)
(use wiimote)
(use util.list)
(use sequencer)
(use gauche.sequence)

(define *frames* 0)
(define *t0*	 0)
(define *width* 0)
(define *height* 0)
(define *wii* #f)
(define *max-value* 0)
(define *last-note* #f)
(define *seq* #f)
(define *port* #f)
(define *channel* 0)
(define *program* 0)
(define *last-buttons* 0)

(define (draw)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-push-matrix)
  (let1 s (max (+ (quotient *width* 100)  1)
	       (+ (quotient *height* 100) 1))
    (let* ((w (wiimote-get-state *wii*))
           (acc (map x->number (coerce-to <list> (ref w 'acc))))
           (pos (map (cut /. <> 256) acc)))
      (set! *max-value* (max *max-value* (apply + acc)))

      (when (not (= (x->number (ref w 'buttons)) *last-buttons*))
        (set! *last-buttons* (x->number (ref w 'buttons)))
        (cond [(not (zero? (logand *last-buttons* 2)))
               (set! *program* (modulo (+ *program* 1) 128))]
              [(not (zero? (logand *last-buttons* 1)))
               (set! *program* (modulo (+ *program* -1) 128))])
        #?=*program*
        (seq-program-change *seq* *port* *channel* *program*)
        )
              
      ;;(when (= (ref w 'ext_type) CWIID_EXT_MOTIONPLUS)
      ;;#?=(map x->number (ref* w 'ext 'motionplus 'angle_rate))
      (when (< (ref acc 0) 20)
        (when *last-note*
          (*last-note*))
        (let1 args (list *seq* *port* *channel* (quotient (ref acc 1) 2) 100)
          (apply seq-note-on args)
          (seq-flush *seq*)
          (set! *last-note*
                (lambda()
                  (apply seq-note-off args)
                  (seq-flush *seq*)))))

      (gl-color 1 0 0)
      (gl-push-matrix)
      (gl-translate 0
                    (*. *height* (/. (apply + acc) *max-value*))
                    0)
      (gl-begin GL_QUADS)
      (gl-vertex 0 0)
      (gl-vertex 0 s)
      (gl-vertex s s)
      (gl-vertex s 0)
      (gl-end)
      (gl-pop-matrix)
      (gl-color 1 1 1)
      (gl-translate (*. *width*  (ref pos 0))
                    (*. *height* (ref pos 1))
                    0)
      (gl-translate (* -0.5 s)
                    (* -0.5 s)
                    0)
      (gl-begin GL_QUADS)
      (gl-vertex 0 0)
      (gl-vertex 0 s)
      (gl-vertex s s)
      (gl-vertex s 0)
      (gl-end)
      (gl-pop-matrix)
      (glut-swap-buffers)
      (inc! *frames*)))
  
  (let1 t (glut-get GLUT_ELAPSED_TIME)
	(when (>= (- t *t0*) 5000)
	      (let1 seconds (/ (- t *t0*) 1000.0)
		    (print #`",*frames* in ,seconds seconds = ,(/ *frames* seconds) FPS")
		    (set! *t0*	   t)
		    (set! *frames* 0)))))

;; new window size or exposure
(define (reshape width height)
  (set! *width* width)
  (set! *height* height)
  (gl-viewport 0 0 width height)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0 width 0 height 0 100)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity))

;; exit upon ESC 
(define (key k x y)
  (when (= k (char->integer #\escape))
	(error "escape pressed")))

(define (visible vis)
  (if (= vis GLUT_VISIBLE)
      (glut-idle-func glut-post-redisplay)
      (glut-idle-func #f)))

(define (main args)
  (set! *seq* (seq-open))
  (set! *port* (seq-make-port *seq* "port 0" "128:0"))
  (set! *wii* (wiimote-open))
  (unwind-protect
   (begin
     (glut-init args)
     (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB))
     (glut-create-window "wiimote board test")
     (newline)
     (print #`"GL_RENDERER	  = ,(gl-get-string GL_RENDERER)")
     (print #`"GL_VERSION	  = ,(gl-get-string GL_VERSION)")
     (print #`"GL_VENDOR	  = ,(gl-get-string GL_VENDOR)")
     (print #`"GL_EXTENSIONS = ,(gl-get-string GL_EXTENSIONS)")
     (newline)
     #|(gl-enable GL_BLEND)
     (gl-enable GL_LINE_SMOOTH)
     (gl-enable GL_POLYGON_SMOOTH)|#
     (glut-display-func	draw)
     (glut-reshape-func	reshape)
     (glut-keyboard-func	key)
     (glut-visibility-func visible)
     (glut-main-loop)
     0)
   #?=(begin
        (when *last-note*
          (*last-note*))
        (seq-close *seq*)
        (wiimote-close *wii*))))
