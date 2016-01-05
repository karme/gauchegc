;;;
;;;
;;;
(define-module ggc.util
  (export pie inexact-split pianissimo  myread
          with-temporary-file
          retry-on-error
          for-all
          )
)
(select-module ggc.util)

;;;
;;; FOR-ALL proc list1 list2 ...
;;;
(define (for-all proc . params)
  (if (null? params)
      (proc)
      (for-each (lambda (e)
		  (apply for-all (cut proc e <...>) (cdr params)))
		(car params))))

;;;
;;; RETRY-ON-ERROR
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
;;; WITH-TEMPORARY-FILE
;;;
;;; To change tempfile templete, do something like,
;;; (with-module ggc.util (set! wtf-tmpl "/tmp/xxx"))
;;;
(define wtf-tmpl "gomi")

(define (with-temporary-file writer reader)
  (receive (port file) (sys-mkstemp wtf-tmpl)
    (unwind-protect
        (begin
          (writer port)
          (close-output-port port)
          (reader file))
      (lambda ()
       (unless (port-closed? port)
         (close-port port))
       (sys-unlink file)))))

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
;;; pie: print inexact number with n digits below decimal point.
;;;
(define (inexact-split d n)
  (define (p d n negative?)
    (let* ((a (floor->exact d))
	   (b (round->exact (* (- d a) (expt 10 n)))))
      (cond ((= b (expt 10 n)) (values (+ a 1) 0 negative?))
            ((> b (expt 10 n)) (error "something is wrong"))
            (else              (values a b negative?)))))
  (if (< d 0)
      (p (- d) n #t)
      (p d     n #f)))

(define (pie d n)
  (receive (a b negative?) (inexact-split d n)
    (if negative? (display "-"))
    (display a)
    (display ".")
    (format #t #"~~~|n|,'0d~~%" b)))

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
