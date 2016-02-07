;;;
;;;
;;;
(define-module ggc.util
  (export direct-product
          direct-product-for-each
          for-all
          pie %f %w
          pianissimo myread
          with-temporary-file
          retry-on-error
          )
)
(select-module ggc.util)

;;;
;;;   direct-product
;;;
;;;   NB: cartesian-product in util.combiations is basically
;;;       same function with slightly different interface.
;;;       cartesian-product-for-each does not have manual entry
;;;       but it is in util.combinations.
;;;
(define (direct-product-for-each proc . params)
  (if (null? params)
      (proc)
      (for-each (lambda (e)
		  (apply direct-product-for-each (cut proc e <...>) (cdr params)))
		(car params))))

(define (direct-product proc . params)
  (if (null? params)
    (list (proc))
    (append-map (lambda (e)
                  (apply direct-product (cut proc e <...>) (cdr params)))
                (car params))))

;; for-all : depreciated. direct product is better name.
(define (for-all proc . params)
  (if (null? params)
      (proc)
      (for-each (lambda (e)
		  (apply for-all (cut proc e <...>) (cdr params)))
		(car params))))

;;;
;;;   RETRY-ON-ERROR
;;;
(define (retry-on-error interval count thunk)
  (define (retry e)
    (cond ((<= count 0)  #f)
          (else
           ;;(format (current-error-port)
           ;;        "retrying ~a~%" count)
           (sys-sleep interval)
           (retry-on-error interval (- count 1) thunk))))
  (with-error-handler retry thunk))

;;;
;;;   WITH-TEMPORARY-FILE
;;;
;;;   To change tempfile templete, do something like,
;;;   (with-module ggc.util (set! wtf-tmpl "/tmp/xxx"))
;;;
(define wtf-tmpl "gomi")

(define (with-temporary-file writer reader)
  (receive (port file) (sys-mkstemp wtf-tmpl)
    (unwind-protect
        (begin
          (writer port)
          (close-output-port port)
          (reader file))
      (unless (port-closed? port)
        (close-port port))
      (sys-unlink file))))

;;;
;;; PIANISSIMO : A minimum pretty-print
;;;
;;; To change indent level do something like,
;;; (with-module ggc.util (set! pp-indent-level 4))
;;;
(define pp-indent-level 2)

(define (pianissimo v)
  (define (ff v n)
    (let ((sp (make-string n #\space)))
      (define (wri x) (display sp) (write x) (newline))
      (define (dsp x) (display sp) (display x) (newline))
      (for-each (lambda (x)
                  (if (list? x)
                      (begin 
                        (dsp "(")
                        (ff x (+ n pp-indent-level))
                        (dsp ")"))
                      (wri x)))
                v)))
  (newline)
  (display "(")(newline)
  (ff v pp-indent-level)
  (display ")")(newline)
  )

;;;
;;; pie: print inexact real number with n digits below decimal point.
;;;
(define (inexact-split d n)
  (define (p d n neg?)
    (let* ((a (floor->exact d))
	   (b (round->exact (* (- d a) (expt 10 n)))))
      (cond ((= b (expt 10 n)) (values (+ a 1) 0 neg?))
            ((> b (expt 10 n)) (error "something is wrong"))
            (else              (values a b neg?)))))
  (if (negative? d)
    (p (- d) n #t)
    (p d     n #f)))

(define (pie d n :optional (positive-sign #f))
  (receive (a b neg?) (inexact-split d n)
    (if neg? (display #\-)
        (if positive-sign
          (display positive-sign)))
    (display a)
    (display #\.)
    (format #t "~v,'0d~%" n b)))

;;;
;;; %f and %w are useful in with string-interpolation syntax
;;;
;;; #"~(%f 3.141592 2)" =>  "3.14"
;;; (let ((s "foo\n")) #"~(%w s)") => "\"foo\\n\""
;;;
(define (%f d n :optional (positive-sign #f))
  (receive (a b neg?) (inexact-split d n)
    (let ((ab (list (number->string a) "."
                    (format "~v,'0d" n b))))
      (apply string-append
             (if neg? (cons "-" ab)
                 (if positive-sign
                   (cons (x->string positive-sign) ab)
                   ab))))))

(define (%w x) (with-output-to-string (lambda () (write x))))

;;;
;;; MYREAD :
;;;
(define (myread)

  (define tcs #[0-9A-Za-z_?<>.\-+])

  (define (skip-spaces)
    (let loop ((c (peek-char)))
      (if (and (not (eof-object? c))
               (char-set-contains? #[[:space:]] c))
          (begin
            (read-char)
            (loop (peek-char))))))

  (define (skip-line)
    (let loop ((c (peek-char)))
      (if (and (not (eof-object? c))
               (not (char=? c #\newline)))
          (begin
            (read-char)
            (loop (peek-char))))))

  (define (read-string)
    ;; バックスラッシュ(\)の処理は省略.
    (let loop ((c (peek-char))
               (s '()))
      (if (eof-object? c)
          (error "missing double quote")
          (if (char=? #\" c)
              (begin 
                (read-char)
                (apply string (reverse s)))
              (begin
                (read-char)
                (loop (peek-char) (cons c s)))))))

  (define (read-as-string c s)
    (if (eof-object? c)
        (apply string (reverse s))
        (if (not (char-set-contains? tcs c))
            (apply string (reverse s))
            (begin
              (read-char)
              (read-as-string (peek-char) (cons c s))))))

  (define (read-pair)
    ;; read-pair というものの . には対応しない.
    (let loop ((o (myread))
               (l '()))
      (if (and (char? o) (char=? #\) o))
          (reverse l)
          (loop (myread) (cons o l)))))

  (skip-spaces)
  (let lp ((c (read-char)))
    (cond 
     ((eof-object? c) c)
     ((char=? #\( c)  (read-pair))
     ((char=? #\) c) c) ; 閉じカッコが余計にあってもエラーにしない.
     ((char=? #\" c)  (read-string))
     ((char-set-contains? tcs c)
      (read-as-string (peek-char) (list c)))
     ((char=? #\; c)  
      (skip-line) 
      (myread))
     (else
      (error "unacceptable character" c)))))

(provide "ggc/util")
