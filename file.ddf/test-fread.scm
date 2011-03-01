(use ggc.file.ddf-fread)
;;;
;;;
(define (test str input)

  (define (rd c istr)
    (if (pair? c)
        ((car c) istr)
        (with-input-from-string istr c)))

  (write str) 
  (display "  from  ")
  (write input)
  (display "\n-->")
  (let* ((x (with-module ggc.file.ddf-fread
              (with-input-from-string str read-format)))
         (y (begin
              (write x)
              (display "\n==>")
              (with-module ggc.file.ddf-fread
              (cmpl x))))
         (r (begin
              (write y)
              (display "\n>>>")
              (rd y input))))
    (write r)
    (display "\n===")
    (write (ddf-fread-from-string str input))
    (newline)))

(test "(I,A,R)" #*" 3\x1fABDC\x1f0.934\x1e")
(test "(2I(4),4A(3),R(4))" #*" 2344321AAABBBCCCDDD1.34\x1e")
(test "(B(32))" #*"xxxx\x1e")
(test "((2B(32)))" #*"xxxxyyyyxxxxyyyyxxxxyyyy\x1e")
(test "()" #*"xxxxyyyyxxxxyyyyxxxxyyyy\x1e")

(print #x78787878) ; xxxx
(print #x79797979) ; yyyy
