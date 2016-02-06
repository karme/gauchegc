;;;
;;; MIRRORING-INPUT
;;;                                   written by skimu.

(define-module ggc.port.mirroring
  (use gauche.vport)
  (export-all)
  )
(select-module ggc.port.mirroring)

#|
(define (open-mirroring-input port dest-port)
  (open-input-buffered-port
   (lambda (n)
     (let ((c (read-char port)))
       (if (eof-object? c)
	   c
	   (begin
	     (display c dest-port)
	     (string c)))))
   4))
|#

(define (open-mirroring-input in dest-port)
  (let ((mi (make <virtual-input-port>)))
    (define (getb)
      (let ((b (read-byte in)))
        (cond ((eof-object? b) b)
              (else
               (write-byte b dest-port)
               b))))
    (define (getc)
      (let ((c (read-char in)))
        (cond ((eof-object? c) c)
              (else
               (write-char c dest-port)
               c))))
    (define (gets n)
      (let ((str (read-block n in)))
        (cond ((eof-object? str) str)
              (else
               (display str dest-port)
               str))))
    (define (ready char?)
      (if char?
        (char-ready? in)
        (byte-ready? in)))
    (define (seek offset whence)
      (port-seek in offset whence))
    (slot-set! mi 'getb getb)
    (slot-set! mi 'getc getc)
    (slot-set! mi 'gets gets)
    (slot-set! mi 'ready ready)
    (slot-set! mi 'seek seek)
    mi))

(define (open-mirroring-output out dest-port)
  (let ((mo (make <virtual-output-port>)))
    (define (putb b)
      (write-byte b dest-port)
      (write-byte b out))
    (define (putc c)
      (write-char c dest-port)
      (write-char c out))
    (define (puts str)
      (display str dest-port)
      (display str out))
    (define (flus)
      (flush dest-port)
      (flush out))
    (define (seek offset whence)
      (port-seek out offset whence))
    (slot-set! mo 'putb  putb)
    (slot-set! mo 'putc  putc)
    (slot-set! mo 'puts  puts)
    (slot-set! mo 'flush flus)
    (slot-set! mo 'seek  seek)
    mo))


(define (call-with-mirroring-input port dest-port proc)
  (let1 p (open-mirroring-input port dest-port)
    (unwind-protect (proc p)
      (unless (port-closed? p)
        (close-port p)))))

(define (with-input-from-port/mirroring-to-port port dest-port thunk)
  (call-with-mirroring-input
      port dest-port
    (lambda (p) (with-input-from-port p thunk))))

(define (with-input-from-port/mirroring-to-file port dest-file thunk)
  (call-with-output-file dest-file
    (lambda (dest-port)
      (with-input-from-port/mirroring-to-port
          port dest-port
        thunk))))

(define (with-input-from-file/mirroring-to-port file dest-port thunk)
  (call-with-input-file file
    (lambda (port)
      (with-input-from-port/mirroring-to-port
          port dest-port
        thunk))))

(define (with-input-from-file/mirroring-to-file file dest-file thunk)
  (call-with-input-file file
    (lambda (port)
      (call-with-output-file dest-file
        (lambda (dest-port)
          (with-input-from-port/mirroring-to-port
              port dest-port
            thunk))))))

(define (with-input-from-string/mirroring-to-port str dest-port thunk)
  (call-with-input-string str
    (lambda (port)
      (with-input-from-port/mirroring-to-port
          port dest-port
        thunk))))

(define (with-input-from-string/mirroring-to-file str dest-file thunk)
  (call-with-input-string str
    (lambda (port)
      (call-with-output-file dest-file
        (lambda (dest-port)
          (with-input-from-port/mirroring-to-port
              port dest-port
            thunk))))))

(provide "ggc/port/mirroring")
