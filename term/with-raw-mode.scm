;;;  $Id: with-raw-mode.scm,v 1.1 2004/07/07 04:05:25 skimu Exp $
;;;
;;;

(define-module ggc.term.with-raw-mode
  (use gauche.termios)
  (export with-raw-mode)
)
(select-module ggc.term.with-raw-mode)

(define (with-raw-mode thunk . args)
  (let* ((port  (or (get-keyword :port args #f)
                    (open-input-file "/dev/tty")))
         (buf   (port-buffering port))
         (attr  (sys-tcgetattr port))
         (bkup  (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag))
         (iflag (slot-ref attr 'iflag))
         (cflag (slot-ref attr 'cflag))
         (oflag (slot-ref attr 'oflag)))
    (dynamic-wind
        (lambda ()
          (set! (port-buffering port) :none)
          (slot-set! attr 'lflag 
                     (logand lflag
                             (lognot (logior ECHO ICANON IEXTEN ISIG))))
          (slot-set! attr 'iflag
                     (logand iflag
                             (lognot (logior BRKINT ICRNL INPCK ISTRIP IXON))))
          (slot-set! attr 'cflag
                     (logand cflag (lognot (logior CSIZE PARENB))))
          (slot-set! attr 'cflag 
                     (logior cflag CS8))
;         (slot-set! attr 'oflag (logand oflag (lognot OPOST)))
          (sys-tcsetattr port TCSAFLUSH attr))
        (lambda () (with-input-from-port port thunk))
        (lambda ()
          (set! (port-buffering port) buf)
          (sys-tcsetattr port TCSANOW bkup)
          (if (not (get-keyword :port args #f))
              (close-input-port port))))))

(provide "ggc/term/with-raw-mode")
