(define-module bar
  (use profile)
  (use foo)
  (export bar-proc
          bar-error-proc))

(select-module bar)

(define (bar-proc a b)
  #?=(foo-proc)
  (dotimes (i 1000)
    #?=(foo-proc))
  #?=(sys-nanosleep 5e8)
  #?=(string-append a b))

(define (bar-error-proc)
  (error "some error"))
