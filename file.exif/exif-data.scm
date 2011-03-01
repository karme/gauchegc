;;;
;;; Exif tag names etc...
;;;
;;; Reference
;;;
;;; TsuruZoh Tachibanaya, Exif file format, Rev 1.4, Feb. 3, 2001,
;;; (http://park2.wakwak.com/~tsuruzoh/Computer/Digicams/exif.html).
;;;
(select-module ggc.file.exif)

(define ifd-tags (make-hash-table))
(for-each (lambda (x)
            (hash-table-put! ifd-tags (car x) (cdr x)))
          '(
            ;;
            ;; IFD0
            ;; 
            (#x010e image-description      ascii-string)
            (#x010f make                   ascii-string)
            (#x0110 model                  ascii-string)
            (#x0112 orientation            uint16)
            (#x011a x-resolution           urational)
            (#x011b y-resolution           urational)
            (#x0128 resolution-unit        uint16)
            (#x0131 software               ascii-string)
            (#x0132 date-time              ascii-string)
            (#x013e white-point            urational)
            (#x013f primary-chromaticities urational)
            (#x0211 YCbCr-coefficients     urational)
            (#x0213 YCbCr-positining       uint16)
            (#x0214 reference-black-white  urational)
            (#x8298 copyright              ascii-string)
            (#x8769 exif-IFD-pointer       uint32)
            ;;
            ;; Exif SubIFD
            ;;
            (#x829a exposure-time             urational)
            (#x829d f-number                  urational)
            (#x8822 exposure-program          uint16)
            (#x8827 ISO-speed-ratings         uint16)
            (#x9000 exif-version              byte)
            (#x9003 date-time-original        ascii-string)
            (#x9004 date-time-digitized       ascii-string)
            (#x9101 conponents-configuration  byte)
            (#x9102 compressed-bits-per-pixel urational)
            (#x9201 shutter-speed-value       srational)
            (#x9202 aperture-value            urational)
            (#x9203 brightness-value          srational)
            (#x9204 exposure-bias-value       srational)
            (#x9205 max-aperture-value        urational)
            (#x9206 subject-distance          singed-rational)
            (#x9207 metering-mode             uint16)
            (#x9208 light-source              uint16)
            (#x9209 flash                     uint16)
            (#x920a focal-length              urational)
            (#x927c maker-note                byte)
            (#x9286 user-comment              byte)
            (#x9290 subsec-time               ascii-string)
            (#x9291 subsec-time-original      ascii-string)
            (#x9292 subsec-time-digitized     ascii-string)
            (#xa000 flash-pix-version         byte)
            (#xa001 color-space               uint16)
            (#xa002 exif-image-width          uint16 uint32)
            (#xa003 exif-image-height         uint16 uint32)
            (#xa004 related-sound-file        ascii-string)
            (#xa005 interoperability-IFD-pointer uint32)
            (#xa20e focal-plane-x-resolution  urational)
            (#xa20f focal-plane-y-resolution  urational)
            (#xa210 focal-plane-resolution-unit uint16)
            (#xa215 exposure-index            urational)
            (#xa217 sensing-method            uint16)
            (#xa300 file-source               byte)
            (#xa301 scene-type                byte)
            (#xa302 CFA-pattern               byte)
            ;;
            ;; Interoperability IFD
            ;;
            (#x0001 interoperability-index ascii-string)
            (#x0002 interoperability-version byte)
            (#x1000 related-image-file-format ascii-string)
            (#x1001 related-image-width uint16 uint32)
            (#x1001 related-image-length uint16 uint32) ; XXX #x1002???
            ;;
            ;; IFD1 
            ;;
            (#x0100 image-width               uint16 uint32)
            (#x0101 image-length              uint16 uint32)
            (#x0102 bits-per-sample           uint16)
            (#x0103 compression               uint16)
            (#x0106 photometric-interpretaion uint16)
            (#x0111 strip-offsets             uint16 uint32)
            (#x0112 orientation               uint16)
            (#x0115 sample-per-pixel          uint16)
            (#x0116 raws-per-strip            uint16 uint32)
            (#x0117 strip-byte-counts         uint16 uint32)
            (#x011a x-resolution              urational)
            (#x011b y-resolution              urational)
            (#x011c planar-configuration      uint16)
            (#x0128 resolution-unit           uint16)
            (#x0201 jpeg-interchange-format   uint32)
            (#x0202 jpeg-interchange-format-length uint32)
            (#x0211 YCbCr-coefficients        urational)
            (#x0212 YCbCr-sub-sampling        uint16)
            (#x0213 YCbCr-positioning         uint16)
            (#x0214 reference-black-while     urational)
            ;;
            ;; misc 
            ;;
            (#x00fe new-subfile-type  uint32)
            (#x00ff subfile-type      uint16)
            (#x012d transfer-function uint16)
            (#x013b artist            ascii-string)
            (#x013d predictor         uint16)
            (#x013e white-point       urational)
            ;; there is more in the web-page...
            ))

(define (find-n tag tag-infos)
  (cond ((null? tag-infos)  #f)
        ((hash-table-get (car tag-infos) tag #f)
         => (lambda (x) x))
        (else (find-n tag (cdr tag-infos)))))

(define (print-exif-tag tag val tag-infos)

  (define (print-rat x)
    (display #\space)
    (display (car x))
    (display #\/)
    (display (cdr x))
    )
  
  (define (print-byte x)
    (display #\space)
    (write (integer->char x)))


  (define (print-dump x)

    (define (print-address a)  (format #t "   ~4'0X:" a))
    (define (print-data  d)    (format #t " ~2'0X"  d))
    (define (print--data d)    (format #t "-~2'0X"  d))

    (define (print-asc asc)
      (if (null? asc) 
          #t
          (let ((c (integer->char (car asc))))
            (if (char-set-contains? #[[:print:]] c)
                (display c)
                (display #\.))
            (print-asc (cdr asc)))))

    (print-address 0)
    (let lp ((adr 0)
             (col 16)
             (asc '())
             (dat x))
      (cond ((null? dat) 
             (display #\space)
             (dotimes (_ (* col 3)) (display #\space))
             (print-asc (reverse asc))
             (newline))
            ((<= col 0)
             (display #\space)
             (print-asc (reverse (cons (car dat) asc)))
             (newline)
             (print-address adr)
             (print-data (car dat))
             (lp (+ adr 1) 15 (list (car dat)) (cdr dat)))
            ((= col 8)
             (print--data (car dat))
             (lp (+ adr 1) 
                 (- col 1) 
                 (cons (car dat) asc) 
                 (cdr dat)))
            (else
             (print-data (car dat))
             (lp (+ adr 1) 
                 (- col 1) 
                 (cons (car dat) asc) 
                 (cdr dat))))))

  (define (print-val val)
    (case (car val)
      ((ascii-string)
       (display #\space)
       (display  (apply string (cdr val))))
      ((urational srational)
       (display #\space)
       (display #\()
       (display  (car val))
       (for-each print-rat (cdr val))
       (display #\)))
      ((byte)
       (if (< (length val) 20)
           (begin
             (display #\space)
             (display "(byte")
             (for-each print-byte (cdr val))
             (display #\)))
           (begin 
             (newline)
             (print-dump (cdr val)))))
      (else
       (format #t " ~50,,,,50:s" val))))

  (let ((n (find-n tag tag-infos)))
    (if n
        (begin
          (format #t "~a(~4'0X):" (car n) tag)
          (print-val val)
          (if (not (memq (car val) (cdr n)))
              (format #t " !!! type mismatch. Expecting ~a~%" (cdr n))
              (newline)))
        (begin
          (format #t "~4'0X:" tag)
          (print-val val)
          (newline)))))


(define (maker-note-dummy tiff-data exif-ifd endian)  #f)

(define (maker-note-getter&tags maker model)
  (cond 
   ((string=? maker "FUJIFILM\0")
    (values maker-note-fujifilm fujifilm-tags))
   (else
    (values maker-note-dummy ifd-tags))))

; EOF
