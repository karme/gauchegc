;;;
;;;
;;;
(use ggc.file.exif)

(define (get-maker-note maker model tiff-data exif-ifd endian)
  (receive (getter tags) (maker-note-getter&tags maker model)
    (values (getter tiff-data exif-ifd endian)
            tags)))
      

(define (test-exif file)

  (define (dump ifds tag-infos)
    (for-each (lambda (ifd)
                (for-each-tag ifd
                              (lambda (tag val) 
                                (print-exif-tag tag val tag-infos)))
                (format #t "--~%"))
              ifds))
  
  (receive (tiff-data endian) (get-tiff-data&endian-from-file file)

    (let* ((ifds (root-ifds tiff-data endian))
           (exif (exif-subifds tiff-data (car ifds) endian))
           (inop #f)
           (maker (get-maker (car ifds)))
           (model (get-model (car ifds)))
           )

      (format #t "maker=~s, model=~s~%" maker model)

      (if exif 
          (set! inop (interoperability-subifds 
                      tiff-data (car exif) endian)))
      (if exif
          (receive (mki mki-tags) 
              (get-maker-note maker model tiff-data (car exif) endian)
            (format #t "========== APP1 IFDs ===========~%")
            (dump ifds (list ifd-tags))
            (format #t "========== EXIF SUB IFD ========~%")
            (dump exif (list ifd-tags))
            (format #t "========== EXIF INTEROPERABILITY IFD =======~%")
            (if inop (dump inop (list ifd-tags)))
            (format #t "========== EXIF MAKER-NOTE IFD =============~%")
            (if mki  (dump mki (list mki-tags ifd-tags))))))))

#|
(class-of (string #\A #\B #\null)) -> string (not imcomplete-string)
(map integer->char '(48 49 48 48)) -> (#\0 #\1 #\0 #\0)
|#


(define (main args)
  (cond ((= (length args) 2)
         (test-exif (cadr args)))
        ((= (length args) 3)
         (with-module ggc.file.exif (set! debug #t))
         (test-exif (cadr args)))
        (else
         (error "Usage: exif-test file")))
  0)

