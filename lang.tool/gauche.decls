(begin
 (define-macro (def-macro form . body)
    `(define-macro ,form (let () ,@body)))
    
 (define pprint (lambda (obj) (write obj) (newline)))
 (define lalr-keyword? symbol?)
  (def-macro (BITS-PER-WORD) 30)
  (def-macro (logical-or x . y) `(logior ,x . ,y))
  (def-macro (lalr-error msg obj) `(error "lalr-parser" ,msg ,obj))
  )