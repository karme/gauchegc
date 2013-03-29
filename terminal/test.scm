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
  (use terminal-output)
  (use terminal-size)
  (use gauche.termios)
  (use gauche.selector)
  (use buffer)
  (use gauche.sequence)
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
      ;; (sys-setenv "GC_PRINT_STATS" "1")
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
                                      -I../video
                                      )
                                    (list (car args))
                                    ;; todo: filter parsed args
                                    (cdr args)))
                   :fork #f)))))

(define-macro (debug-assert e)
  `(if (not ,e)
     (error "Assertion failed: " ,(x->string e))))

(define *rows* 36)
(define *cols* 120)
(define *terminal-out* #f)
(define *status-line* "scmterm")
(define *viewport-y* 0)

(define (viewport-y)
  *viewport-y*)

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

(define (pty-setup cmd args)
  (receive (pid fd) (sys-forkpty-and-exec cmd args :term #f)
    #?=(list pid fd)
    (values (open-input-fd-port fd :buffering :none)
            (open-output-fd-port fd :buffering :full))))

;; todo: there should be a builtin no?
(define (copy-port-nonblock in max . outports)
  (when (and (char-ready? in) (> max 0))
    (let ((c (read-char in)))
      (for-each
       (lambda(out)
         ;; todo: might block => proc name misleading
         (write-char c out))
       outports))
    (apply copy-port-nonblock (append (list in (- max 1)) outports))))

(define (host-terminal-setup port)
  (if (sys-isatty port)
    (let* ((attr (sys-tcgetattr port))
           (lflag (slot-ref attr 'lflag)))
      ;; put into raw mode and disable echo
      (slot-set! attr
                 'lflag (logand lflag
                                (lognot ICANON)
                                (lognot ECHO)))
      (sys-tcsetattr port TCSAFLUSH attr)
      ;; return a closure to restore the tty
      (lambda()
        (slot-set! attr 'lflag lflag)
        (sys-tcsetattr port TCSANOW attr)))
    (lambda())))

(define (terminal-text)
  (ref (ref *terminal-out* 'buffer) 'text))

(define (map-keysym keysym)
  (case keysym
    [(K_UP)    "[A"]
    [(K_DOWN)  "[B"]
    [(K_RIGHT) "[C"] 
    [(K_LEFT)  "[D"]
    [else #f]))

(define (clip v minv maxv)
  (debug-assert (not (< maxv minv)))
  (min (max minv v) maxv))

(define (viewport-scroll-by x)
  (set! *viewport-y* (clip (+ *viewport-y* x)
                           0
                           (max (- (size-of (terminal-text)) *rows*) 0)))
  ;;(screen-dirty!)
  )

(define (viewport-scroll-up)
  (viewport-scroll-by (- *rows*)))

(define (viewport-scroll-down)
  (viewport-scroll-by *rows*))

(define (scroll-to-bottom)
  (let ((target (max (- (size-of (terminal-text)) *rows*) 0)))
    (viewport-scroll-by (- target *viewport-y*))))

(define (on-key-down pty key)
  ;; #?=key
  (let ((c (ucs->char (assoc-ref #?=key 'unicode)))
        (escape (cut write-char #\escape)))
    (with-output-to-port pty
      (lambda()
        ;; left alt sends escape => take a look at the modifiers!
        ;; todo: only together with other non-modifier keys
        (when (any (cut eq? 'LALT <>) (assoc-ref key 'modifiers))
          (escape))
        (cond [(not (equal? c #\null))
               (write-char c)]
              [(map-keysym (assoc-ref key 'symname))
               (escape)
               (display (map-keysym (assoc-ref key 'symname)))]
              [(and (any (cut eq? 'RSHIFT <>) (assoc-ref key 'modifiers))
                    (eq? (assoc-ref key 'symname) 'K_PAGEUP))
               (viewport-scroll-up)]
              [(and (any (cut eq? 'RSHIFT <>) (assoc-ref key 'modifiers))
                    (eq? (assoc-ref key 'symname) 'K_PAGEDOWN))
               (viewport-scroll-down)]
              [else
               #?=`("todo: unhandled keypress" ,key)])
        (flush)))))

(define (current-text)
  (let ((text (terminal-text)))
    (map-with-index (lambda(idx row)
                      (format "~5d ~a"
                              (+ idx (viewport-y))
                              (apply string (map integer->char row))))
                    (subseq text
                            (viewport-y)
                            (min (+ (viewport-y) *rows*)
                                 (size-of text))))))

(define (on-terminal-text-size-changed)
  (scroll-to-bottom))

(define (main-2 args)
  (sys-putenv "TERM" "vt100")
  (receive (in out) (if (null? (cdr args))
                      ;;(pty-setup "gosh" '("gosh"))
                      (pty-setup "emacs" '("emacs" "-nw" "-q"))
                      (pty-setup (cadr args) (cdr args)))
    (let* ((stdin (current-input-port))
           (stdout (current-output-port))
           (host-terminal-restore (host-terminal-setup stdin))
           (selector (make <selector>)))
      (unwind-protect
       (begin
         #?=(get-terminal-size #?=(port-file-number out))
         #?=(set-terminal-size! #?=(port-file-number out) *cols* *rows*)
         #?=(get-terminal-size (port-file-number out))
         (set! *terminal-out* (open-output-terminal (make-buffer *cols* *rows*)))
         (selector-add! selector
                        stdin
                        (lambda (port r)
                          ;; todo: maybe just copy filtered output to another buffer!
                          (when (char-ready? port)
                            (set! *status-line* (format "»~x«" (char->integer (peek-char port)))))
                          (copy-port-nonblock port *cols* out)
                          (flush out))
                        '(r))
         (selector-add! selector
                        in
                        (lambda (port r)
                          ;; (copy-port-nonblock port 100 stdout *terminal-out*)
                          (let ((old-size (size-of (terminal-text))))
                            (copy-port-nonblock port (* *cols* 2) *terminal-out*)
                            (flush stdout)
                            (when (not (= old-size (size-of (terminal-text))))
                              (on-terminal-text-size-changed))
                            ;;(screen-dirty!)
                            ))
                        '(r))
         ;; todo: ugly hack
         ;; (selector-add! selector 3 (lambda _ (sgachine-poll-input)) '(r))

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
             (let* ((some-text "X")
                    (font-width (car (string-bbox some-text)))
                    (font-height (cadr (string-bbox some-text)))
                    (newline (cute gl-translate 0 (- font-height) 0))
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
                               (on-resize w h)]
                              [(key-down)
                               (on-key-down out (cadr e))])))
                   (selector-select selector 0)
                   (gl-clear GL_COLOR_BUFFER_BIT)
                   ;; (gl-rect x 0 (+ x 10) h)
                   (gl-push-matrix*
                    ;;(gl-translate (+ 0 (modulo (* 2 x) w)) (+ 0 (modulo x h)) 0)
                    (gl-translate 0 (- h font-height) 0)
                    ;; try to estimate number of missed frames
                    (when (> x 100)
                      (let ((frame-time (time->seconds (time-difference new-time last-time)))
                            (avg-frame-time (/ (time->seconds uptime) frames)))
                        (when (> (/ frame-time avg-frame-time) 1.5)
                          (inc! frames-missed #?=(- (round->exact (/ frame-time avg-frame-time)) 1)))))
                    (let1 s (time->seconds uptime)
                      (draw-string (format #f "~s frames in ~a seconds ⇒ ~a frames/s, ~a frames missed, ~a gc runs"
                                           frames
                                           (float-format s)
                                           (float-format (/ frames s))
                                           frames-missed
                                           (gc-get-gc-no)))
                      (newline))
                    (for-each
                     (lambda(l)
                       (draw-string l)
                       (newline))
                     (current-text)))
                   (when (odd? (quotient frames 10))
                     (gl-push-matrix*
                      (gl-translate (* font-width  (+ 5 1 (ref* *terminal-out* 'buffer 'x)))
                                    (+ h
                                       (* font-height (+ -2
                                                         (- (* (ref* *terminal-out* 'buffer 'y)))
                                                         (viewport-y))))
                                    0)
                      (gl-rect 0 0 font-width font-height)))
                   (gl-swap-buffers)
                   (set! last-time new-time)
                   (inc! frames)
                   (set! x (modulo (+ x 1) w))))))))
       (host-terminal-restore))))
  0)
