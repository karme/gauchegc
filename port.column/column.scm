;;;
;;;  column-port : linenumber and column aware port.
;;;

(define-module ggc.port.column
  (use gauche.vport)
  (export-all)
  )
(select-module ggc.port.column)

(define-class <column-port> (<virtual-input-port>)
  (src name #;line column last-column))

(define (open-column-port src-port)
  (let ((port (make  <column-port>)))
    (define (getc)
      (let ((c (read-char src-port)))
        (cond ((eof-object? c) c)
              ((char=? #\nl c)
               #;(inc! (~ port'line))
               (set! (~ port'last-column) (~ port'column))
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
    (slot-set! port 'last-column 0)
    (slot-set! port 'getc   getc)
    (slot-set! port 'ready  ready)
    port))

(define-method port-name ((port <column-port>))
  (slot-ref port'name))

(define-method port-current-column ((port <column-port>))
  (let ((cc ((with-module gauche.internal %port-ungotten-chars) port)))
    (case (length cc)
      ((0) (slot-ref port'column))
      ((1) (if (char=? #\newline (car cc))
             (slot-ref port'last-column)
             (- (slot-ref port'column) 1)))
      (else
       (error "port-current-column: unsupported gauche version")))))

(define-method port-current-column ((port <port>)) #f)

(define (port-current-line$ port)
  (let ((ln (port-current-line port)))
    (if (negative? ln)
      ln
      (let ((cc ((with-module gauche.internal %port-ungotten-chars) port)))
        (case (length cc)
          ((0) ln)
          ((1) (if (char=? #\newline (car cc))
                 (- ln 1)
                 ln))
          (else
           (error "port-current-line$ : unsupported gauche version")
           ;; or this may work....
           #;(- ln (length (filter (cut char=? #\newline) cc)))
           ))))))

;; <virtual-input-port> takes care of this.
#;(define-method port-current-line ((port <column-port>))
      (slot-ref port'line))

(define (call-with-input-port/column s proc)
  (let ((p (open-column-port s)))
    (unwind-protect (proc p)
      (unless (port-closed? p) (close-port p)))))

(define (with-input-from-port/column s thunk)
  (call-with-input-port/column s
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-input-from-file/column file thunk)
  (call-with-input-file file
    (lambda (p)
      (with-input-from-port/column p thunk))))

(provide "ggc/port/column")
