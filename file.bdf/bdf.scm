;;;
;;; ggc.file.bdf  an ad-hoc BDF reader
;;;
(define-module ggc.file.bdf
  (use srfi-13)
  (export-all)
)
(select-module ggc.file.bdf)

(define (bdf-read-glyph name)

  (define (bdf-read-hexnum n)
    (let lp ((n n)
             (r '()))
      (if (= n 0) 
          (reverse r)
          (lp (- n 1) (cons (string->number (read-line) 16) r)))))

  (define (is? str name)
    (eq? 0 (string-scan str name)))

  (define (make-entry l)
    (let ((ll (string-tokenize l)))
      (cons (string->symbol (car ll)) 
            (map string->number (cdr ll)))))

  (define (bitmap-length #f))

  (let lp ((l (read-line))
           (r (list name '|CHAR|)))
    (cond ((is? l "ENDCHAR") (reverse r))

          ((is? l "BBX")
           (let ((e (make-entry l)))
             (set! bitmap-length (list-ref e 2))
             (lp (read-line) (cons e r))))

          ((or (is? l "ENCODING")
               (is? l "SWIDTH")
               (is? l "DWIDTH"))
           (lp (read-line) 
               (cons (make-entry l) r)))
          
          ((is? l "BITMAP")
           (if (not bitmap-length)
               (error "No BBX found before BITMAP"))
           (let ((bml (bdf-read-hexnum bitmap-length)))
             (lp (read-line) (cons (cons '|BITMAP| bml) r))))

          (else 
           (format (current-error-port) "Ignored: ~s~%" l)
           (lp (read-line) r)))))

(define (read-bdf)

  (define (bdf-read-properties n)
    (let lp ((n n))
      (cond ((eq? 0 (string-scan (read-line) "ENDPROPERTIES"))
             (if (= n 0) 
                 '(PROPERTIES "IGNORED FOR NOW")
                 (error "Something wrong, maybe input corrupted")))
            (else (lp (- n 1))))))

  (define (is? str name)
    (eq? 0 (string-scan str name)))

  (define (getnumparams l)
    (map string->number (cdr (string-tokenize l))))

  (let ((hdr (read-line)))
    (if (not (eq? 0 (string-scan hdr "STARTFONT")))
        (error "File is not BDF"))

    (let lp ((l (read-line))
             (r (list (list '|STARTFONT| (cadr (string-tokenize hdr))))))
      (cond ((is? l "ENDFONT") (reverse r))

            ((is? l "STARTPROPERTIES ")
             (let ((prop (bdf-read-properties 
                          (string->number (cadr (string-tokenize l))))))
               (lp (read-line) (cons prop r))))

            ((is? l "STARTCHAR ")
             (let ((chl (bdf-read-glyph (cadr (string-tokenize l)))))
               (lp (read-line) (cons chl r))))

            ((is? l "COMMENT ")
             (lp (read-line) 
                 (cons (list '|COMMENT| (substring l 7 (string-length l)))
                       r)))

            ((is? l "FONT ")
             (let ((ll (string-tokenize l)))
               (lp (read-line) (cons (list '|FONT| (cadr ll)) r))))

            ((is? l "FONTBOUNDINGBOX ")
             (lp (read-line)
                 (cons (cons '|FONTBOUNDINGBOX| 
                             (getnumparams l))
                       r)))

            ((is? l "SIZE ")
             (let ((ll (string-tokenize l)))
               (lp (read-line) 
                   (cons (cons '|SIZE| (getnumparams l))
                         r))))

            ((is? l "CHARS ")
             (let ((ll (string-tokenize l)))
               (lp (read-line)
                   (cons (cons '|CHARS| (getnumparams l))
                         r))))
            (else
             (format (current-error-port) "Ignored: ~s~%" l)
             (lp (read-line) r))))))

;;;
;;;
;;;
(define (bdf-version bdf)         (cadr (assq '|STARTFONT| bdf)))
(define (bdf-font bdf)            (cadr (assq '|FONT| bdf)))
(define (bdf-size bdf)            (cdr  (assq '|SIZE| bdf)))
(define (bdf-fontboundingbox bdf) (cdr  (assq '|FONTBOUNDINGBOX| bdf)))
(define (bdf-chars bdf)           (cadr (assq '|CHARS| bdf)))

(define (bdf-find-char bdf pred)
  (let lp ((ent bdf))
    (cond ((null? ent) #f)
          ((eq? (caar ent) '|CHAR|)
           (if (pred (car ent))
               (car ent)
               (lp (cdr bdf))))
          (else (lp (cdr bdf))))))

(define (bdf-find-char-by-encoding bdf encoding)
  (bdf-find-char bdf 
                 (lambda (char)
                   (eq? (assq '|ENCODING| (cddr char)) encoding))))

(define (bdf-find-char-by-name bdf name)
  (bdf-find-char bdf 
                 (lambda (char)
                   (string=? (bdf-char->name char) name))))

(define (bdf-char-name char)     (cadr char))
(define (bdf-char-encoding char) (cadr (assq '|ENCODING| char)))
(define (bdf-char-swidth char)   (cdr  (assq '|SWIDTH| char)))
(define (bdf-char-dwidth char)   (cdr  (assq '|DWIDTH| char)))
(define (bdf-char-bbx char)      (cdr  (assq '|BBX| char)))
(define (bdf-char-bitmap char)   (cdr  (assq '|BITMAP| char)))

(define (for-each-char proc bdf)
  (let lp ((p bdf))
    (cond ((null? p) #t)
          ((eq? (caar p) '|CHAR|)
           (proc (car p))
           (lp (cdr p)))
          (else (lp (cdr p))))))


(provide "ggc/file/module")

;;; EOF