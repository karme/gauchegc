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
  (use srfi-19)
  (use gc-hack)
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
      (sys-setenv "GC_PRINT_STATS" "1")
      (sys-setenv "GC_ENABLE_INCREMENTAL" "1")
      (sys-setenv "GC_PAUSE_TIME_TARGET" "5")
      (run-process #?=(cons 'gosh
                            (append `(
                                      -fload-verbose
                                      -Frecursed
                                      ,#`"-F,|video-backend|"
                                      -I.
                                      -I../sdl
                                      -I../input
                                      -I../ftgl
                                      -I../gc-hack
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

(define (float-format f)
  (when (< f 0) (error "todo"))
  (receive (q r) (quotient&remainder (round->exact (* f 100)) 100)
    (format #f "~d.~2'0d" q r)))

(define (main-2 args)
  (receive (w h)
      (apply values (gl-get-size))
    (gl-disable GL_DEPTH_TEST)
    ;; (gl-enable GL_LINE_SMOOTH)
    ;; (gl-enable GL_POLYGON_SMOOTH)
    (on-resize w h)
    (let* ((x 0)
           (quit? #f)
           ;; todo: use fontconfig
           (font-spec '(ftgl "/usr/share/fonts/truetype/unifont/unifont.ttf" 18))
           (draw-string (apply font-get-string font-spec))
           (string-bbox (apply font-get-string-bbox font-spec))
           (frames 0)
           (start-time (current-time))
           (frames-missed 0))
      (let* ((some-text "Hello friend of (), λ and gauche (ゴーシュ) scheme ☻")
             (text-height (cadr (string-bbox some-text)))
             (newline (cute gl-translate 0 (- text-height) 0))
             (last-time #f))
        (while (not quit?)
          (let* ((new-time (current-time))
                 (uptime (time-difference new-time start-time)))
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
             (gl-translate (+ 0 (modulo (* 2 x) w)) (+ 0 (modulo x h)) 0)
             (draw-string some-text)
             (newline)
             (draw-string #`",|x|∈ℕ")
             (newline)
             (let1 s (time->seconds uptime)
               (draw-string (format #f "~s frames in ~a seconds ⇒ ~a frames/s"
                                    frames
                                    (float-format s)
                                    (float-format (/ frames s)))))
             (newline)
             ;; try to estimate number of missed frames
             (when (> x 100)
               (let ((frame-time (time->seconds (time-difference new-time last-time)))
                     (avg-frame-time (/ (time->seconds uptime) frames)))
                 (when (> (/ frame-time avg-frame-time) 1.5)
                   (inc! frames-missed #?=(- (round->exact (/ frame-time avg-frame-time)) 1)))))
             (draw-string #`",|frames-missed| frames missed (likely due to gc)")
             (newline)
             (draw-string #`",(gc-get-gc-no) gc runs"))
            (gl-swap-buffers)
            (set! last-time new-time)
            (inc! frames)
            (set! x (modulo (+ x 1) w)))))))
  0)
