(define-module foo
  (export foo-proc))

(select-module foo)

(define (foo-proc)
  #?=(string-append "f" "o" "o"))
