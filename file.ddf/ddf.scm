;;;
;;; DDF : Data Descriptive File (ISO 8211)
;;;
;;; NOTE
;;;   This implementation is intended to read SPDF/DLG DDF 
;;;   files.
;;;
;;; References
;;;   http://www.3dartist.com/WP/sdts/sdtsnotes.htm
;;;   http://www.jst.go.jp/SIST/handbook/sist11/index.htm
;;;
(define-module ggc.file.ddf
  (use gauche.uvector)
  (use ggc.file.ddf-fread)
  (export 
   ;; Primary API
   read-ddf  ddf-get-value
   
   ;; Low-level API
   read-ddr read-dr
   ddr-get-name
   ddr-get-labels
   ddr-get-format
   dr-get-records dr-get-data
   )
)
(select-module ggc.file.ddf)

;;;
;;;  DEBUGGING TOOLS
;;;
(define debug #f)

(define (dmes . args)
  (if debug (apply format args)))

(define (print-dirs dirs data)

  (define (dsp x)
    (display x)
    (display #\space))

  (define (wri x)
    (write x)
    (display #\space))

  (let lp ((dirs dirs))
    (if (null? dirs)
        #t
        (receive (tag siz ptr) (dir.tag&siz&ptr (car dirs))
          (dsp tag)
          (wri (substring data ptr (+ ptr siz)))
          (newline)
          (lp (cdr dirs))))))

;;;
;;;  DDR/DR LEADER
;;;

;; DDR/DR directory entry
(define (make-dir tag siz len) (list tag siz len))
(define (dir.tag dir)         (list-ref dir 0))
(define (dir.siz dir)         (list-ref dir 1))
(define (dir.ptr dir)         (list-ref dir 2))
(define (dir.tag&siz&ptr dir) (apply values dir))

;;
;; READ-LEADER : 
;;   reads DR/DDR leader and returns leader-id, size of 
;;   remaining record in bytes and directories (list of 
;;   directory entry).
;;
(define (read-leader)

  (define (str->number str start end)
    (string->number (substring str start end)))

  (let ((s (read-block 24)))

    (dmes #t "READ-LEADER: ~s " s)

    (if (eof-object? s)

        (values s s s)

        (let ((record-len (str->number s 0 5))
              ;;(interchange-level (str->number s 5 6))
              (leader-id  (integer->char (string-byte-ref s 6)))
              ;;(field-control-label-len (str->number s 10 12))
              ;;(ddr-dda-pointer (str->number  s 12 17))
              (siz-len (str->number s 20 21))
              (ptr-len (str->number s 21 22))
              (tag-len (str->number s 23 24)))

          (let lp ((c    (peek-byte))
                   (ndir 0)
                   (l    '()))
            (cond
             ((eof-object? c)
              (error "unexpected end-of-file"))

             ((= #x1e c) 
              (read-byte)
              (dmes #t "~%")
              (values leader-id  
                      (- record-len
                         (+ 25 (* ndir (+ ptr-len siz-len tag-len)))
                         )        ; ddr-dda-size
                      (reverse l) ; dirs
                      ))

             (else
              ;; Gauche can make a symbol such as |0000|
              (let ((tag (string->symbol (read-block tag-len)))
                    (siz (string->number (read-block siz-len)))
                    (ptr (string->number (read-block ptr-len))))
                (dmes #t "~a " tag)
                (lp (peek-byte)
                    (+ ndir 1)
                    (cons (make-dir tag siz ptr) l))))))))))

;;;
;;;  Data Descriptive Record and Data Record
;;;

(define (trans-data dirs data)
  (let lp ((dirs dirs)
           (alis '()))

    (if (null? dirs)
        (reverse alis)
        (receive (tag siz ptr) (dir.tag&siz&ptr (car dirs))
          (lp (cdr dirs)
              (cons
               (list tag
                     (substring data ptr (+ ptr siz)))
               alis))))))

;;
;; READ-DDR :
;;    reads Data Descriptive Record and
;;    returns an association list of tag and data, 
;;    i.e., ((tag1 data) (tag2 data2) ...), 
;;    where tag and data are symbol and incomplete-string,
;;    respectively
;;
(define (read-ddr)
  (receive (leader-id data-size dirs)  
      (read-leader)

    (cond
     ((eof-object? leader-id)
      (error "unexpected end-of-file"))

     ((char=? leader-id #\L)
      (trans-data dirs (read-block data-size)))

     (else
      (error "unexpected leader-id" leader-id)))))

;;
;; READ-DRS :
;;    reads Data Records and returns a list of association 
;;    list of tag and data.
;;    For most case, null list '() will be passed by 
;;    application programs.
;;
(define (read-drs drs)
  (receive (leader-id data-size dirs)  
      (read-leader)

    (cond
     ((eof-object? leader-id)
      (reverse drs))

     ((char=? leader-id #\R)
      (let lp ((data (read-block data-size))
               (drs  drs))
        (if (eof-object? data)
            (reverse drs)
            (let ((x (trans-data dirs data)))
              (lp (read-block data-size)
                  (cons x drs))))))
     
     ((char=? leader-id #\D)
      (read-drs (cons (trans-data dirs (read-block data-size))
                       drs)))

     (else
      (error "unexpected leader-id for data record" leader-id)))))

;;;
;;; DDF File
;;;

;;
;; READ-DDF :
;;  read a DDF file and returns what read-ddr and read-drs
;;  returns.
;;
(define (read-ddf)
  (let* ((ddr (read-ddr))
         (drs (read-drs '())))
    (values ddr drs)))

;;;
;;; ETC.
;;;
(define (ddr-get-name tag ddr)
  (let ((x (assq tag ddr)))
    (cond
     ((not x)        "")
     (else
      (list-ref (split-record (cadr x)) 0)))))


(define (ddr-get-labels tag ddr)
  (let ((x (assq tag ddr)))
    (cond
     ((not x) '())
     ((eq? (car x) '|0001|) '(RCID))
     (else
      (map string->symbol
             (string-split (list-ref (split-record (cadr x)) 1) #\!))))))

(define (ddr-get-format tag ddr)
  (let ((x (assq tag ddr)))
    (cond
     ((not x)        "()")
     ((eq? (car x) '|0001|) "(I)")
     (else
      (list-ref (split-record (cadr x)) 2)))))


(define (ddf-get-value tag dr ddr)
  (let ((fmt (ddr-get-format tag ddr))
        (str (dr-get-data   tag dr)))
    (if str 
        (ddf-fread-from-string fmt str)
        '())))

;;
(define (split-record istr)
  (let* ((len (string-size istr))
         (str (string-incomplete->complete 
               (substring istr 0 (- len 1)))))
    (if str 
        (string-split str #\x1f)
        str)))
;;
;;
;;
(define (dr-get-records tag dr)
  (let ((x (assq tag dr)))
    (if x 
        (split-data (cadr x)) 
        x)))
;;
;;
;;
(define (dr-get-data tag dr)
  (let ((x (assq tag dr)))
    (if x 
        (cadr x)
        x)))

(provide "ggc/file/ddf")
