;;;  $Id: without-echo.scm,v 1.1 2004/07/07 04:05:25 skimu Exp $
;;;
;;;

(define-module ggc.term.without-echo
  (use gauche.termios)
  (export without-echo)
)
(select-module ggc.term.without-echo)

(define (without-echo thunk . args)
  (let* ((prompt (get-keyword :prompt args #f))
         (port   (or (get-keyword :port args #f)
                     (current-input-port)))
         (attr (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag)))
    (if prompt 
        (begin (display prompt)
               (flush)))
    (dynamic-wind
        (lambda ()
          (slot-set! attr 'lflag (logand lflag (lognot ECHO)))
          (sys-tcsetattr port TCSAFLUSH attr))
        thunk
        (lambda ()
          (slot-set! attr 'lflag lflag)
          (sys-tcsetattr port TCSANOW attr)))))

(provide "ggc/term/without-echo")
