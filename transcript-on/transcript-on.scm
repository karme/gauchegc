(define-module ggc.transcript-on
  (use ggc.port.mirroring)
  (require "gauche/interactive")
  (export-all)
  )
(select-module ggc.transcript-on)

(define (transcript-on file)
  (let* ((dest (open-output-file file))
         (mi   (open-mirroring-input  (current-input-port)  dest))
         (mo   (open-mirroring-output (current-output-port) dest)))
    (unwind-protect
        (with-ports mi mo mo
          (lambda ()
            (read-eval-print-loop
             (lambda ()
               (let ((x (with-module gauche.interactive (%reader))))
                 (if (equal?  x '(transcript-off))
                   (eof-object)
                   x)))
             #f #f
             (lambda ()
               (display #"[~|file|]> ")
               (flush)))))
      (close-port mo)
      (close-port mi)
      (close-port dest))))

(provide "ggc/transcript-on")
;;; EOF
