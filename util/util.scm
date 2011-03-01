;;;
;;;
;;;
(define-module ggc.util
  (export pianissimo myread
          with-temporary-file
          )
)
(select-module ggc.util)

;;;
;;; WITH-TEMPORARY-FILE
;;;
;;; To change tempfile templete do something like,
;;; (with-module ggc.util (set! wtf-tmpl "/tmp/xxx"))
;;;
(define wtf-tmpl "gomi")

(define (with-temporary-file writer reader)
  (receive (port file) (sys-mkstemp wtf-tmpl)
    (dynamic-wind
     (lambda () #f)
     (lambda () 
       (writer port)
       (close-output-port port)
       (reader file))
     (lambda ()
       (if (not (port-closed? port))
           (close-output-port port))
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
                  (if (pair? x)
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
