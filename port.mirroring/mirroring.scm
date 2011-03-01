;;; $Id: mirroring.scm,v 1.2 2005/07/04 21:50:33 skimu Exp $

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

(define (open-mirroring-input port dest-port)
  (let ((mirror-port (make <virtual-input-port>)))
    (define (getb)
      (let ((c (read-byte port)))
        (if (eof-object? c)
            c
            (begin
              (write-byte c dest-port)
              c))))
    (define (getc)
      (let ((c (read-char port)))
        (if (eof-object? c)
            c
            (begin
              (display c dest-port)
              c))))
    (define (gets n)
      (let ((str (read-block n port)))
        (if (eof-object? str)
            c
            (begin
              (display str dest-port)
              str))))
    (define (ready char?)
      (if char? 
          (char-ready? port)
          (byte-ready? port)))
    (define (seek offset whence)
      (port-seek port offset whence))
  
    (slot-set! mirror-port 'getb getb)
    (slot-set! mirror-port 'getc getc)
    (slot-set! mirror-port 'gets gets)
    (slot-set! mirror-port 'ready ready)
    (slot-set! mirror-port 'seek seek)
    mirror-port))

(define (call-with-mirroring-input port dest-port proc)
  (let ((p #f))
    (dynamic-wind
	(lambda () 
	  (set! p (open-mirroring-input port
					dest-port)))
	(lambda () (proc p))
	(lambda () 
	  (close-input-port p)))))

(define (with-input-from-port/mirroring-to-port port dest-port thunk)
  (call-with-mirroring-input port
			     dest-port
			     (lambda (p) 
			       (with-input-from-port p thunk))))

(define (with-input-from-port/mirroring-to-file port dest-file thunk)
  (call-with-output-file dest-file
    (lambda (dest-port)
      (with-input-from-port/mirroring-to-port port
                                              dest-port 
                                              thunk))))

(define (with-input-from-file/mirroring-to-port file dest-port thunk)
  (call-with-input-file file 
    (lambda (port)
      (with-input-from-port/mirroring-to-port port
                                              dest-port 
                                              thunk))))

(define (with-input-from-file/mirroring-to-file file dest-file thunk)
  (call-with-input-file file
    (lambda (port)
      (call-with-output-file dest-file
        (lambda (dest-port)
          (with-input-from-port/mirroring-to-port port
                                                  dest-port
                                                  thunk))))))

(define (with-input-from-string/mirroring-to-port str dest-port thunk)
  (call-with-input-string str
    (lambda (port)
      (with-input-from-port/mirroring-to-port port
                                              dest-port 
                                              thunk))))

(provide "ggc/port/mirroring")
