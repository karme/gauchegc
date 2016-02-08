;;;
;;;  column-port : linenumber and column aware port.
;;;

(define-module ggc.port.column
  (use gauche.vport)
  (export-all)
  )
(select-module ggc.port.column)

(define-class <column-port> (<virtual-input-port>)
  (src name #;line column))

(define (open-column-port src-port)
  (let ((port (make  <column-port>)))
    (define (getc)
      (let ((c (read-char src-port)))
        (cond ((eof-object? c) c)
              ((char=? #\nl c)
               #;(inc! (~ port'line))
               (set! (~ port'column) 0)
               c)
              (else
               (inc! (~ port'column))
               c))))
    (define (ready char?)
      (if char?
        (char-ready? src-port)
        (byte-ready? src-port)))
    (slot-set! port 'src  src-port)
    (slot-set! port 'name (port-name src-port))
    #;(slot-set! port 'line
               (let ((x (port-current-line src-port)))
                 (if (negative? x) 1 x)))
    (slot-set! port 'column 0)
    (slot-set! port 'getc   getc)
    (slot-set! port 'ready  ready)
    port))

(define-method port-name ((port <column-port>))
  (slot-ref port'name))

(define-method port-current-column ((port <column-port>))
  (slot-ref port'column))

;; <virtual-input-port> takes care of this.
#;(define-method port-current-line ((port <column-port>))
      (slot-ref port'line))

(define (call-with-input-file/column file proc)
  (let*  ((s (open-input-file  file))
          (p (open-column-port s)))
    (unwind-protect (proc p)
      (unless (port-closed? p) (close-port p))
      (unless (port-closed? s) (close-port s)))))

(define (with-input-from-file/column file thunk)
  (call-with-input-file/column file
    (lambda (p)
      (with-input-from-port p thunk))))

(provide "ggc/port/column")
