(define-module bar
  (use debug-verbose)
  (export bar-proc))

(select-module bar)

(define (bar-proc)
  #?=(string-append "f" "o" "o"))
