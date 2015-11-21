;;;
;;;
;;;
(define-module ggc.term.with-raw-mode
  (use gauche.termios)
  (export with-raw-mode)
)
(select-module ggc.term.with-raw-mode)

;;; In SunOS 5.8
;;;
;;;    $ stty min 1 time 0
;;;
;;;  is required before using of this function.
;;;
(define (with-raw-mode thunk . args)
  (let* ((port  #f) (bsave #f) (asave #f) (attr  #f)
         (lflag #f) (iflag #f) (cflag #f) (oflag #f))
    (dynamic-wind
        (lambda ()
          (set! port   (or (get-keyword :port args #f) (open-input-file "/dev/tty")))
          (set! bsave  (port-buffering port))
          (set! asave  (sys-tcgetattr  port))
          (set! attr   (sys-tcgetattr  port))
          (set! lflag  (slot-ref attr 'lflag))
          (set! iflag  (slot-ref attr 'iflag))
          (set! cflag  (slot-ref attr 'cflag))
          (set! oflag  (slot-ref attr 'oflag))
          (set! (port-buffering port) :none)
          (slot-set! attr 'lflag (logand lflag (lognot (logior ECHO ICANON IEXTEN ISIG))))
          (slot-set! attr 'iflag (logand iflag (lognot (logior BRKINT ICRNL INPCK ISTRIP IXON))))
          (slot-set! attr 'cflag (logand cflag (lognot (logior CSIZE PARENB))))
          (slot-set! attr 'cflag (logior cflag CS8))
        #;(slot-set! attr 'oflag (logand oflag (lognot OPOST)))
          (sys-tcsetattr port TCSAFLUSH attr))
        (lambda () (with-input-from-port port thunk))
        (lambda ()
          (set! (port-buffering port) bsave)
          (sys-tcsetattr port TCSANOW asave)
          (if (not (get-keyword :port args #f)) (close-input-port port)))
      )))

(provide "ggc/term/with-raw-mode")
