;;;
;;; DDF-FREAD
;;;
;;; The format string resembles that of fortrans, and
;;; input stream is terminated by either eof or #x1e,
;;; to read ISO-8211 file.
;;;
;;; NOTE
;;; This module will be replaced by ggc.file.format-read 
;;; which has more generic and powerfull APIs.
;;; 
;;; Examples
;;;
;;; "(A)"     : variable length ascii string
;;; "(R)"     : variable length real number (in ascii)
;;; "(A(10))" : an ascii string of 10 characters
;;; "(I(6))"  : a 6-digit integer
;;; "(B(32))" : a 32-bit signed integer in binary.
;;;             (assuming HFMT="BI32")
;;; "(A(5),I(6))" 
;;;           : an ascii 10-character string and 
;;;             a 6-digit integer.
;;; "(4I(6),3R(8))"
;;;           : four 6-digit integer and three real numbers
;;;             of which length is 8 in bytes.
;;; "((2B(32)))"
;;;           : a list of two 32-bit unsigned integer
;;;
;;; variable length format specifiers and specified length 
;;; specifiers can not be mixed. Followings are error.
;;;
;;;    "(A,I(5))", "(I,I(6))", "(R(6),A)".
;;;
;;; Input stream for the variable length format specifiers
;;; should be partitioned by #x1f, for example, input stream
;;; for format string "(A,I,R)" should be something like
;;;
;;;    #*"ABRACADABRA\x1f   36\x1f3.141592".
;;;

(define-module ggc.file.ddf-fread
  (use binary.io)
  (use srfi-13)
  (export ddf-fread
          ddf-fread-from-string
          )
)
(select-module ggc.file.ddf-fread)

(define (cmpl l)

  (define (string->number2 str)
    (string->number (string-trim str)))

  (define (make-B n)
    (case n
      ((32) (lambda () 
              (read-binary-sint32 (current-input-port) 'big-endian)))
      ((16) (lambda () 
              (read-binary-sint16 (current-input-port) 'big-endian)))
      ((8)  (lambda () 
              (read-binary-sint8  (current-input-port) 'big-endian)))
      (else
       (error "unsupported B format" n))))

  (define (make-A n)
    (lambda ()
      (string-incomplete->complete (read-block n))))

  (define (make-I n)
    (lambda ()
      (string->number2 (string-incomplete->complete (read-block n)))))

  (define (make-R n)
    (lambda ()
      (string->number2 (string-incomplete->complete (read-block n)))))

  (define (split-data istr)
    (let* ((len (string-size istr))
           (str (string-incomplete->complete 
                 (substring istr 0 (- len 1)))))
      (if str 
          (string-split str #\x1f)
          str)))

  (define (make-ppr lis)
    (let ((fs (map (lambda (x)
                     (case (car x)
                       ((#\B) (error "unsupported"))
                       ((#\A) 
                        (lambda (x) x))
                       ((#\I #\R)
                        (lambda (x) (string->number2 x)))
                       (else
                        (error "unknown format spec"))))
                   lis)))
      (lambda (istr)
        (map (lambda (f x)
               (f x))
             fs
             (split-data istr)))))

  (define (partitioned? l)
    (cond ((null? l)      #t)
          ((cadar l)      #f)
          (else
           (partitioned? (cdr l)))))

  (cond 
   ((null? l)
    (lambda () '()))

   ((pair? (caar l))
    (let ((code (cmpl (car l))))
      (lambda ()
        (let lp ((l '()))
          (let ((c (peek-byte)))
            (if (or (eof-object? c) 
                    (eq? #x1e c))
                (reverse l)
                (lp (cons (code) l))))))))

   ((partitioned? l)  (list (make-ppr l)))
   (else
    (let ((codes (map (lambda (fn)
                        (case (car fn)
                          ((#\A) (make-A (cadr fn)))
                          ((#\B) (make-B (cadr fn)))
                          ((#\I) (make-I (cadr fn)))
                          ((#\R) (make-R (cadr fn)))))
                      l)))
      (lambda ()
        ;; map-in-order, to be strict
        (map (lambda (x) (x)) codes))))))

(define (read-format)
  ;;
  (define (getoken)
    (let ((c (read-char)))
      (cond
       ((eof-object? c) (error "unexpected end-of-file"))
       ((char-numeric? c) (rdnum (list c)))
       (else c))))
  ;;
  (define (rdnum l)
    (let ((c (peek-char)))
      (if (char-numeric? c)
          (rdnum (cons (read-char) l))
          (string->number (apply string (reverse l))))))

  (define (rdparam)
    (let ((c (peek-char)))
      (cond
       ((eof-object? c) (error "unexpected end-of-file"))
       ((char=? #\( c)
        (read-char)
        (let ((n (rdnum '())))
          (let ((c (read-char)))
            (if (not (eq? c #\)))
                (error "unexpected" c))
            n)))
       (else #f))))

  (define (rdspecs)
    (let lp ((s (getoken))
             (l '()))
      (cond
       ((eq? #\) s)  (reverse l))
       ((eq? #\, s)  (lp (getoken) l))
       ((eq? #\( s)  (let ((x (rdspecs)))
                       (lp (getoken) (cons x l))))
       ((number? s)
        (let* ((sp    (getoken))
               (param (rdparam))
               (spec  (list sp param)))
          (let lp2 ((n s)
                    (l l))
            (if (<= n 0)
                (lp (getoken) l)
                (lp2 (- n 1) (cons spec l))))))
       (else
        (let ((param (rdparam)))
          (lp (getoken) (cons (list s param) l)))))))
  ;;
  (let ((c (getoken)))
    (cond
     ((eq? #\( c)      (rdspecs))
     (else  (error "format should start with (")))))

;;; API
;;;
;;; TODO: cache compiled code
;;;

;;
;; DDF-FREAD-FROM-STRING
;;
(define (ddf-fread-from-string fmt istr)
  (let* ((x (with-input-from-string fmt read-format))
         (c (cmpl x)))
    (if (pair? c)
        ((car c) istr)
        (with-input-from-string istr c))))

;;
;; FORMAT-READ
;;
(define (ddf-fread fmt)
  (let* ((x (with-input-from-string fmt read-format))
         (c (cmpl x)))
    (if (pair? c)
        (error "variable length format is not supported")
        (c))))

(provide "ggc/file/ddf-fread")
