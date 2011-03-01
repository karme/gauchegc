;;;
;;; Read JPEG file.
;;;
(define-module ggc.file.jpeg
  (use gauche.uvector)
  (export SOI EOI APP0 APP1 RSTT0 RST7 SOS
          read-jpeg-marker
          read-jpeg)
  )

(select-module ggc.file.jpeg)

(define debug #f)

(define (dmes . args)
  (if debug (apply format args)))

;;;
;;; JFIF markers
;;;
(define-constant SOI  #xD8)
(define-constant EOI  #xD9)
(define-constant APP0 #xE0)
(define-constant APP1 #xE1)
(define-constant RST0 #xD0)
(define-constant RST7 #xD7)
(define-constant SOS  #xDA)

(define (read-jpeg-marker)

  (define (fmt1? m)
    (or (= m SOI)
        (= m EOI)
        (and (<= RST0 m) (<= m RST7))))

  (let ((ff (read-byte)))
    (if (not (= ff #xFF))
        (error "jpg marker does not start with FF" ff)))

  (let ((m (read-byte)))
    (dmes #t "read-jpg-marker: ~2'0X~%" m)
    (if (fmt1? m) 
        (list m)
        (let* ((x (read-byte))
               (y (read-byte))
               (l (- (+ (* x 256) y) 2))
               (v (make-u8vector l))
               (d (read-block! v)))
          (if (= l d)
              (list m v)
              (error "unexpected end-of-file"))))))

(define (read-jpeg)

  (define (rst? m) (and (<= RST0 m) (<= m RST7)))

  (define (rdi l)
    (let ((d (read-byte)))
      (if (= d #xFF)
          (let ((e (read-byte)))
            (cond
             ((= e EOI)   (reverse l))
             ((= e #x00)  (rdi (cons d l)))
             ((rst? e)    (rdi (cons e (cons d l))))
             (else        (errorf "image data corrupted ~2'0X" e))))
          (rdi (cons d l)))))

  (let lp ((mk (read-jpeg-marker))
           (l '()))
    (if (eq? (car mk) SOS) 
        (reverse (cons (list EOI) (cons (rdi '())
                                        (cons mk l))))
        (lp (read-jpeg-marker) (cons mk l)))))

;;;
;;;
;;;
#|
(define (test-jpeg)

  (define (print-m m)
    (let ((x (cond ((= m SOI) 'SOI)
                   ((= m EOI) 'EOI)
                   ((= m APP0) 'APP0)
                   ((= m APP1) 'APP1)
                   (else m))))
      (if (symbol? x)
          (format #t "~a : " x)
          (format #t "~2'0X : " x))))

  (define (dump s)
    (write s)
    (newline))
      
  (let lp ((j (read-jpeg)))
    (cond 
     ((null? j) 0)
     ((eq? (caar j) SOI)
      (format #t  "SOI~%")
      (lp (cdr j)))
     ((eq? (caar j) EOI)
      (format #t  "EOI~%")
      (lp (cdr j)))
     ((eq? (caar j) SOS)
      (format #t  "SOS~%")
      (lp (cddr j)))
     (else
      (print-m (caar j))
      (dump (cadar j))
      (lp (cdr j))))))
|#
(provide "ggc/file/jpeg")
