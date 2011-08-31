#!/usr/bin/gosh -I.
(use sequencer)
(use srfi-1)

;; (define (main args)
;;   (let* ((seq (seq-open))
;;          (port (seq-make-port seq "port 0" "128:0"))
;;          (channel 6)
;;          (vel 70)
;;          (stepsize 4)
;;          (l #?=(iota (+ (quotient 127 stepsize) 1) 0 stepsize)))
;;     (for-each (lambda(x)
;;                 (seq-note-on seq port channel x vel)
;;                 (seq-flush seq)
;;                 (sys-nanosleep 100000000))
;;               l)
;;     (for-each (lambda(x)
;;                 (seq-note-off seq port channel x vel)
;;                 (seq-flush seq))
;;               l)
;;     (seq-close seq)))

(define (main args)
  (let* ((seq (seq-open))
         (port (seq-make-port seq "port 0" "128:0"))
         (channel 0)
         (vel 127)
         (x 60))
    (dotimes (i 32)
      (seq-program-change seq port channel i)
      (seq-note-on seq port channel x vel)
      (seq-flush seq)
      (print "ping")
      (sys-nanosleep 1e9)
      (seq-note-off seq port channel x vel)
      (seq-flush seq)
      (sys-nanosleep 0.1e9))))


