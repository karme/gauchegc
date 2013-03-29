#!/usr/bin/env gosh
#| -*- mode: scheme; coding: utf-8; -*- |#

;; trampoline
(cond-expand
 (recursed
  (use gl-setup)
  (use gl)
  (use input)
  (use file.util)
  (use font)
  (define (main args)
    (unwind-protect
     (begin
       (gl-open)
       (input-open)
       (main-2 args))
     (begin
       (input-close)
       (gl-close)))))
 (else
  (use gauche.process)
  (define (main args)
    ;; todo: parse args / config file whatever
    (let1 video-backend (if #t
                          "video-sdl"
                          "video-osmesa")
      (run-process #?=(cons 'gosh
                            (append `(
                                      -fload-verbose
                                      -Frecursed
                                      ,#`"-F,|video-backend|"
                                      -I.
                                      -I../sdl
                                      -I../input
                                      -I../ftgl
                                      )
                                    (list (car args))
                                    ;; todo: filter parsed args
                                    (cdr args)))
                   :fork #f)))))

(define (on-resize w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0 (- w 1)
            0 (- h 1)
            0 100)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity))

(define (key? e key)
  (and (list? e)
       (eq? (car e) 'key-down)
       (eq? (assoc-ref (cadr #?=e) 'symname)
            key)))

(define (mod-key? e key)
  (and (list? e)
       (member key (assoc-ref (cadr e) 'modifiers))))

(define (main-2 args)
  (receive (w h)
      (apply values (gl-get-size))
    (gl-disable GL_DEPTH_TEST)
    (on-resize w h)
    (let ((x 0)
          (quit? #f)
          ;; todo: use fontconfig
          (draw-string (font-get-string 'ftgl "/usr/share/fonts/truetype/unifont/unifont.ttf" 24))
          (string-bbox (font-get-string-bbox 'ftgl "/usr/share/fonts/truetype/unifont/unifont.ttf" 24)))
      (let* ((some-text "Hello friend of (), λ and gauche (ゴーシュ) scheme ☻")
             (text-height (cadr (string-bbox some-text))))
        (while (not quit?)
          (while (input-poll-event) => e
                 (when (list? e)
                   (case (car e)
                     [(quit)
                      (set! quit? #t)]
                     [(resized)
                      (gl-set-size! (cadr e) (caddr e))
                      (set! w (cadr e))
                      (set! h (caddr e))
                      (on-resize w h)]))
                 (when (key? e 'K_ESCAPE)
                   (set! quit? #t)))
          (gl-clear GL_COLOR_BUFFER_BIT)
          (gl-rect x 0 (+ x 10) h)
          (gl-push-matrix*
           (gl-translate (modulo (* 2 x) w) (modulo x h) 0)
           (draw-string some-text)
           (gl-translate 0 (- text-height) 0)
           (draw-string (format #f "~s∈ℕ" x)))
          (gl-swap-buffers)
          (set! x (modulo (+ x 1) w))))))
  0)
