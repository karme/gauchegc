(define-module ggc.term.vt100
  (use gauche.process)
  (export-all)
)
(select-module ggc.term.vt100)

;;;
;;;  ANSI/VT100 Escape codes
;;;
(define (vt100-clear-screen)  (display "\x1b[H\x1b[2J"))
(define (vt100-clear-eol)     (display "\x1b[0K"))
(define (vt100-clear-bol)     (display "\x1b[1K"))
(define (vt100-clear-line)    (display "\x1b[2K"))
(define (vt100-clear-eos)     (display "\x1b[0J"))
(define (vt100-clear-bos)     (display "\x1b[1J"))
(define (vt100-clear-screen)  (display "\x1b[2J"))
(define (vt100-reverse-video) (display "\x1b[7m"))
(define (vt100-normal-video)  (display "\x1b[0m"))
(define (vt100-scroll-up)     (display "\x1b[D"))
(define (vt100-scroll-down)   (display "\x1b[M"))

(define (vt100-hide-cursor)   (display "\x1b[?25l"))
(define (vt100-show-cursor)   (display "\x1b[?25h"))
(define (vt100-cursor-home)   (display "\x1b[H"))
(define (vt100-cursor-position x y) (format #t "\x1b[~d;~dH" y x))
(define (vt100-cursor-up    n)      (format #t "\x1b[~dA" n))
(define (vt100-cursor-down  n)      (format #t "\x1b[~dB" n))
(define (vt100-cursor-right n)      (format #t "\x1b[~dC" n))
(define (vt100-cursor-left  n)      (format #t "\x1b[~dD" n))

;;;
;;; NOTE: resize usually resides /usr/X11/bin
;;;
(define (vt100-get-size)

  (define (get-var&val str)
    ;; "VAR=VAL;"
    (let* ((varval (string-split str #\=)))
      (if (>= (length varval) 2)
          (let ((var (string->symbol (list-ref varval 0)))
                (val (let ((v (list-ref varval 1)))
                       (string->number (substring v 0 (- (string-length v) 1))))))
            (values var val))
          (values #f #f))))
  
  (with-input-from-process "resize -u"
    (lambda ()
      (let lp ((l (read-line))
               (x #f) (y #f))
        (if (eof-object? l)
            (values x y)
            (receive (var val) (get-var&val l)
              (case var
                ((COLUMNS) (lp (read-line) val y))
                ((LINES)   (lp (read-line) x val))
                (else (lp (read-line) x y)))))))))

(provide "ggc/term/vt100")
