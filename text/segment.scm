(define-module ggc.text.segment
  (use srfi-1)
  (use srfi-13)
  (use gauche.vport)
  (export-all)
)
(select-module ggc.text.segment)  

;;;
;;;  BASIC SEGMENTED TEXT OPERATIONS
;;;
(define (text-insert text pos str)
  (let lp ((pos   (if (> pos 0) pos 0))
           (otext text)
           (ntext '()))
    (cond ((null? otext) 
           (if (>= pos 0) 
               (reverse (cons str ntext))
               (reverse ntext)))
          ((< 0 pos (string-length (car otext)))
           (let ((before (substring (car otext) 0 pos))
                 (after  (substring (car otext) 
                                    pos
                                    (string-length (car otext)))))
             (lp -1 
                 (cdr otext) 
                 (cons after (cons str (cons before ntext))))))
          ((= 0 pos)
           (lp -1 
               (cdr otext) 
               (cons (car otext) (cons str ntext))))
          (else
           (lp (- pos (string-length (car otext)))
               (cdr otext)
               (cons (car otext) ntext))))))

(define (text-delete text pos count)
  ;; detele count characters from text position pos
  (let lp ((pos   (if (> pos 0)   pos   0))
           (count (if (> count 0) count 0))
           (otext text)
           (ntext '()))
    (cond ((null? otext) (reverse ntext))
          ((< 0 pos (string-length (car otext)))
           (if (< (+ pos count) (string-length (car otext)))
               (let ((before (substring (car otext) 0 pos))
                     (after  (substring (car otext) 
                                        (+ pos count) 
                                        (string-length (car otext)))))
                 (lp -1 0 
                     (cdr otext) 
                     (cons after (cons before ntext))))
               (lp 0 
                   (- count (- (string-length (car otext)) pos))
                   (cdr otext)
                   (cons (substring (car otext) 0 pos) ntext))))
          ((= pos 0)
           (if (< count (string-length (car otext)))
               (lp -1 0 
                   (cdr otext)
                   (cons (substring (car otext)
                                    count
                                    (string-length (car otext)))
                         ntext))
               (lp 0 
                   (- count (string-length (car otext)))
                   (cdr otext) 
                   ntext)))
          (else
           (lp (- pos (string-length (car otext)))
               count
               (cdr otext) 
               (cons (car otext) ntext))))))

(define (text->string text) (apply string-append text))
(define (text-flatten text) (list (text->string text)))
(define (text-size text)
  (let lp ((text text)
           (acc 0))
    (cond ((null? text) acc)
          (else
           (let ((len (string-length (car text))))
             (lp (cdr text) (+ acc len)))))))

(define (text-count-character-before text pos ch)
  ;; returns number of ch before posistion pos
  (let lp ((text text)
           (pos (if (> pos 0) pos 0))
           (acc 0)
           (nc  0))
    (cond ((null? text) nc)
          ((= pos 0) nc)
          ((< 0 pos (string-length (car text)))
           (let ((x (string-count (car text) ch 0 pos)))
             (+ nc x)))
          (else
           (let ((x   (string-count (car text) ch))
                 (len (string-length (car text))))
             (if x
                 (lp (cdr text) (- pos len) (+ acc len) (+ nc x))
                 (lp (cdr text) (- pos len) (+ acc len) nc)))))))

(define (text-count-character-after text pos ch)
  (let lp ((text text)
           (pos (if (> pos 0) pos 0))
           (acc 0)
           (nc  0))
    (cond ((null? text) nc)
          ((< 0 pos (string-length (car text)))
           (let* ((len (string-length (car text)))
                  (cc  (string-count (car text) ch pos len)))
             (lp (cdr text) (- pos len) (+ acc len) cc)))
          ((<= pos 0)
           (let* ((len (string-length (car text)))
                  (cc  (string-count (car text) ch)))
             (lp (cdr text) (- pos len) (+ acc len) (+ nc cc))))
          (else
           (let ((len (string-length (car text))))
             (lp (cdr text) (- pos len) (+ acc len) 0))))))

(define (text-find-character-after text pos ch)
  ;; returns position of the first ch after pos and rest of text
  ;; returns text size as second value, if no ch was found.
  (let lp ((text text)
           (pos (if (> pos 0) pos 0))
           (acc 0))
    (cond ((null? text) (values #f acc))
          ((< 0 pos (string-length (car text)))
           (let* ((len (string-length (car text)))
                  (str (substring (car text) pos len))
                  (idx (string-scan str ch)))
             (if idx 
                 (values (+ acc pos idx) 
                         (cons (substring str idx (- len pos)) (cdr text)))
                 (lp (cdr text) (- pos len) (+ acc len)))))
          ((<= pos 0)
           (let* ((len (string-length (car text)))
                  (idx (string-scan (car text) ch)))
             (if idx
                 (values (+ acc idx)
                         (cons (substring (car text) idx len) (cdr text)))
                 (lp (cdr text) (- pos len) (+ acc len)))))
          (else
           (let ((len (string-length (car text))))
             (lp (cdr text) (- pos len) (+ acc len)))))))

(define (text-find-character-before2 text pos ch)
  ;; returns posision of the last ch before posistion pos
  ;; and text from there to the end
  ;; returns text size as second value, if pos is larger than
  ;; text size
  ;; returns #f and a copy of text if no ch is found
  (let lp ((text  text)
           (pos (if (> pos 0) pos 0))
           (acc 0)
           (p   #f)
           (atext '()))
    (cond ((null? text) (values p acc)) ; pos is out of range
          ((= pos 0)    (values p (append (reverse atext) text)))
          ((< 0 pos (string-length (car text)))
           (let* ((str (car text))
                  (len (string-length str))
                  (idx (string-index-right str ch 0 pos)))
             (if idx
                 (values (+ acc idx) 
                         (append (reverse (cons (substring str idx len)
                                                 atext))
                                  (cdr text)))
                 (values p (append (reverse atext) text)))))
          (else
           (let* ((str (car text)) 
                  (len (string-length str))
                  (idx (string-index-right str ch)))
             (if idx
                 (lp (cdr text) (- pos len) (+ acc len) (+ acc idx)
                     (list (substring str idx len)))
                 (lp (cdr text) (- pos len) (+ acc len) p 
                     (cons str atext))))))))

(define (text-find-character-before1 text pos ch)
  ;; returns posision of the last ch before posistion pos
  ;; retruns #f if no ch is found
  (let lp ((text text)
           (pos (if (> pos 0) pos 0))
           (acc 0)
           (p #f))
    (cond ((null? text) p)              ; pos is out of range
          ((= pos 0) p)
          ((< 0 pos (string-length (car text)))
           (let* ((str (car text))
                  (len (string-length str))
                  (idx (string-index-right str ch 0 pos)))
             (if idx (+ acc idx) p)))
          (else
           (let* ((str (car text)) 
                  (len (string-length str))
                  (idx (string-index-right str ch)))
             (if idx
                 (lp (cdr text) (- pos len) (+ acc len) (+ acc idx))
                 (lp (cdr text) (- pos len) (+ acc len) p)))))))

(define (text-get-character text pos)
  ;; get char of text at pos.
  ;; if pos is out of range, #f will be returned
  (let lp ((text text)
           (pos (if (> pos 0) pos 0)))
    (cond ((null? text) #f) ; error pos is out of range
          ((= pos 0) (string-ref (car text) 0))
          ((< 0 pos (string-length (car text))) (string-ref (car text) pos))
          (else
           (lp (cdr text) (- pos (string-length (car text))))))))

(define (text-get-text text pos count)
  ;; get count characters of text from posision pos.
  ;; if negative count is passed, text from pos to the end
  ;; of text will be returned.
  (define (get text count r)
    (cond ((null? text) (reverse r))
          ((= count 0) (reverse r))
          ((< 0 count (string-length (car text)))
           (get '() 0 (cons (substring (car text) 0 count) r)))
          (else
           (get (cdr text) 
                (- count (string-length (car text))) 
                (cons (car text) r)))))
  (let lp ((text  text)
           (pos   (if (> pos 0) pos 0)))
    (cond ((null? text) '())
          ((= pos 0) 
           (get text count '()))
          ((< 0 pos (string-length (car text)))
           (get (cons (substring (car text) pos (string-length (car text)))
                      (cdr text)) count '()))
          (else
            (lp (cdr text) (- pos (string-length (car text))))))))
    
(define (text-search-forward text pos str)
  (let lp ((text text)
           (pos  pos)
           (acc  0))
    (receive (start rtext)
        (text-find-character-after text pos (string-ref str 0))
      (if start
          (let ((cnd (text->string (text-get-text rtext 0 
                                                  (string-length str)))))
            (if (string=? cnd str) 
                (values (+ acc start) rtext)
                (lp rtext 1 (+ acc start))))
          (values #f '())))))
                    
(define (text-search-backward text pos str)
  (let* ((len (string-length str))
         (ch  (string-ref str (- len 1))))
    (let lp ((text text)
             (pos  pos))
      (let ((p (text-find-character-before1 text pos ch)))
        (if p
            (let* ((txt (text-get-text text 0 (+ p 1)))
                   (sta (- p (- len 1)))
                   (cnd (text->string (text-get-text txt sta len))))
              (if (string=? cnd str)
                  (values sta txt)
                  (lp txt (- p 1))))
            (values #f '()))))))

(define (text-line-number text pos)
  ;; returns line number of pos
  ;; Line number of the first line is 1
  (+ 1 (text-count-character-before text pos #\newline)))
    
(define (text-number-of-lines text pos)
  ;; Count number of lines after the line of pos
  (+ 1 (text-count-character-after text pos #\newline)))

(define (text-end-of-line text pos count)
  (let lp ((pos    pos)
           (count  count))
    (cond ((< count 0) (error "Negative or invalid count"))
          ((= count 0) (- pos 1))
          (else
           (receive (x s) (text-find-character-after text pos #\newline)
             (if x 
                 (lp (+ x 1) (- count 1))
                 ;; If we can not find enough newline
                 ;; return with end of text
                 s) 
             )))))

(define (text-beginning-of-line text pos count)
  (let lp ((pos   pos)
           (count count))
    (cond ((< count 0) (error "Negative or invalid count"))
          ((= count 0) (+ pos 1))
          (else
           (let ((x (text-find-character-before1 text pos #\newline)))
             (if x 
                 (lp x (- count 1))
                 ;; If we can not find enough newline
                 ;; return with beginning of text
                 0))))))

(define (text-previous-line text pos . count)
  (let ((c (if (null? count) 1 (car count))))
    (let* ((b (text-beginning-of-line text pos 1))
           (o (- pos b))
           (p (text-beginning-of-line text (- b 1) c))
           (e (text-end-of-line text p 1))
           (c (+ p o)))
      (if (< c e) c e))))

(define (text-next-line text pos . count)
  (let ((c (if (null? count) 1 (car count))))
    (let* ((b (text-beginning-of-line text pos 1))
           (o (- pos b))
           (e (text-end-of-line text pos c))
           (x (text-end-of-line text (+ e 1) 1))
           (c (+ e 1 o)))
      (if (< c x) c x))))
    
(define (open-input-text-string-port text pos)
  (let ((str (text->string (text-get-text text pos -1))))
    (open-input-string str)))

(define (open-input-text-virtual-port text pos)
  (let* ((txt (text-get-text text pos -1))
         (pos 0))
    (define (getc)
      (if (null? txt) 
          #f
          (let ((str (car txt)))
            (cond ((< pos (string-length str))
                   (let ((ch (string-ref str pos)))
                     (set! pos (+ pos 1))
                     ch))
                  ((= pos (string-length str))
                   (set! txt (cdr txt))
                   (set! pos 0)
                   (getc))
                  (else (error "something wrong"))))))
    (make <virtual-input-port> :getc getc)))

;;(define open-input-text open-input-text-string-port)
(define open-input-text open-input-text-virtual-port)

(define-class <text-output-port> (<virtual-output-port>)
  ((text  :init-value   '()
          :init-keyword :text
          :accessor text-of)))

(define (open-output-text text pos)
  (let* ((buf  '())
         (pos  pos)
         (port (make <text-output-port> :text text)))
    (define (putc ch)  (set! buf (cons (string ch) buf)))
    (define (puts str) (set! buf (cons str buf)))
    (define (flush)
      (let* ((str (text->string (reverse buf)))
             (len (string-length str)))
        (set! (text-of port) (text-insert (text-of port) pos str))
        (set! buf '())
        (set! pos (+ pos len))))
    (slot-set! port 'putc  putc)
    (slot-set! port 'puts  puts)
    (slot-set! port 'flush flush)
    (slot-set! port 'close flush)
    port))

(provide "ggc/text/segment")
;;; EOF
