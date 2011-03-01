;;;
;;; Read EXIF file.
;;; 
;;; Reference
;;;
;;; TsuruZoh Tachibanaya, Exif file format, Rev 1.4, Feb. 3, 2001,
;;; (http://park2.wakwak.com/~tsuruzoh/Computer/Digicams/exif.html).
;;;
(define-module ggc.file.exif
  (use gauche.uvector)
  (use binary.io)
  (use ggc.file.jpeg)

  (export get-tiff-data&endian-from-file

          <exif-ifd>

          get-tag
          put-tag!
          tag-exists?
          for-each-tag
          get-tag-info
          get-tag-values
          get-tag-value
          get-tag-type
          get-tag-string

          root-ifds
          exif-subifds
          interoperability-subifds

          use-tags

          ;;
          ;; autoloads
          ;; 
          print-exif-tag      ifd-tags
          maker-note-getter&tags
          maker-note-fujifilm fujifilm-tags
          maker-note-nikon  nikon-tags
          maker-note-nikon1 nikon1-tags
          maker-note-olympus  olympus-tags

          ;;
          ;; XXX: Most of the following APIs will be moved
          ;;      to elsewhere.
          ;;

          ;; IFD0
          get-maker
          get-model
          get-orientaion
          get-x-resolution
          get-y-resolution
          get-resolution-unit
          get-software
          get-date-time
          get-image-description
          get-copyright

          ;; EXIF IFD
          get-exposure-time
          get-f-number
          get-exposure-program
          get-iso-speed-rating
          get-exif-version
          get-date-time-original
          get-components-configuration
          get-compressed-bits-per-pixel
          get-shutter-speed-value
          get-aperture-value
          get-brightness-value
          get-Ev
          get-max-aperture-value
          get-exposure-bias-value
          get-subject-distance
          get-metering-mode
          get-light-source
          get-flash
          get-focal-length
          get-flash-pix-version
          get-exif-image-width
          get-exif-image-height
          get-focal-plane-x-resolution
          get-focal-plane-y-resolution
          get-focal-plane-resolution-unit
          )
  )

(select-module ggc.file.exif)

;;;
;;; IFD0 Tags
;;;
(define-constant IMAGE-DESCRIPTION        #x010e)
(define-constant MAKE                     #x010f)
(define-constant MODEL                    #x0110)
(define-constant ORIENTATION              #x0112)
(define-constant X-RESOLUTION             #x011a)
(define-constant Y-RESOLUTION             #x011b)
(define-constant RESOLUTION-UNIT          #x0128)
(define-constant SOFTWARE                 #x0131)
(define-constant DATE-TIME                #x0132)
(define-constant WHITE-POINT              #x013e)
(define-constant PRIMARY-CHROMATICITIES   #x013f)
(define-constant YCbCr-COEFFICIENTS       #x0211)
(define-constant YCbCr-POSITIONING        #x0213)
(define-constant REFERENCE-BLACK-WHITE    #x0214)
(define-constant COPYRIGHT                #x8298)
(define-constant EXIF-IFD-POINTER         #x8769)

;;;
;;; Exif SubIFD Tags
;;;
(define-constant EXPOSURE-TIME            #x829a)
(define-constant F-NUMBER                 #x829d)
(define-constant EXPOSURE-PROGRAM         #x8822)
(define-constant ISO-SPEED-RATING         #x8827)
(define-constant EXIF-VERSION             #x9000)
(define-constant DATE-TIME-ORIGINAL       #x9003)
(define-constant DATE-TIME-DIGITIZED      #x9004)
(define-constant COMPONENTS-CONFIGURATION #x9101)
(define-constant COMPRESS-BITS-PER-PIXEL  #x9102)
(define-constant SHUTTER-SPEED-VALUE      #x9201)
(define-constant APERTURE-VALUE           #x9202)
(define-constant BRIGHTNESS-VALUE         #x9203)
(define-constant EXPOSURE-BIAS-VALUE      #x9204)
(define-constant MAX-APERTURE-VALUE       #x9205)
(define-constant SUBJECT-DISTANCE         #x9206)
(define-constant METERING-MODE            #x9207)
(define-constant LIGHT-SOURCE             #x9208)
(define-constant FLASH                    #x9209)
(define-constant FOCAL-LENGTH             #x920a)
(define-constant MAKER-NOTE               #x927c)
(define-constant USER-COMMENT             #x9286)
(define-constant SUBSEC-TIME              #x9290)
(define-constant SUBSEC-TIME-ORIGINAL     #x9291)
(define-constant SUBSEC-TIME-DIGITIZED    #x9292)
(define-constant FLASH-PIX-VERSION        #xa000)
(define-constant COLOR-SPACE              #xa001)
(define-constant EXIF-IMAGE-WIDTH         #xa002)
(define-constant EXIF-IMAGE-HEIGHT        #xa003)
(define-constant RELATED-SOUND-FILE       #xa004)
(define-constant INTEROPERABILITY-IFD-POINTER #xa005)
(define-constant FOCAL-PLANE-X-RESOLUTION #xa20e)
(define-constant FOCAL-PLANE-Y-RESOLUTION #xa20f)
(define-constant FOCAL-PLANE-RESOLUTION-UNIT #xa210)
(define-constant EXPOSURE-INDEX           #xa215)
(define-constant SENSING-METHOD           #xa217)
(define-constant FILE-SOURCE              #xa300)
(define-constant SCENE-TYPE               #xa301)
(define-constant CFA-PATTERN              #xa302)

;;;
;;;  Interoperability IFD Tags
;;;
(define-constant INTEROPERABILITY-INDEX    #x0001)
(define-constant INTEROPERABILITY-VERSION  #x0002)
(define-constant RELATED-IMAGE-FILE-FORMAT #x1000)
(define-constant RELATED-IMAGE-WIDTH       #x1001)
(define-constant RELATED-IMAGE-LENGTH      #x1002)

;;;
;;;  IFD1 (thumbnail iamge) Tags
;;;
(define-constant IMAGE-WIDTH                    #x0100)
(define-constant IMAGE-LENGTH                   #x0101)
(define-constant COMPRESSION                    #x0103)
(define-constant JPEG-INTERCHANGE-FORMAT        #x0201)
(define-constant JPEG-INTERCHANGE-FORMAT-LENGTH #x0202)

;;;
;;; <exif-ifd> class
;;;
(define-class <exif-ifd> ()
  ((hash :init-value (make-hash-table)
         :accessor hash-of)))

(define-method get-tag ((ifd <exif-ifd>) 
                        (tag <integer>)
                        (func <procedure>))
  (let ((info (hash-table-get (hash-of ifd) tag #f)))
    (if info
        (func info)
        info)))

(define-method put-tag! ((ifd <exif-ifd>)
                         (tag <integer>)
                         val)
  (hash-table-put! (hash-of ifd) tag val))

(define-method tag-exists? ((ifd <exif-ifd>)
                            (tag <integer>))
  (hash-table-exists? (hash-of ifd) tag))

(define-method for-each-tag ((ifd <exif-ifd>)
                             (proc <procedure>))
  (hash-table-for-each (hash-of ifd) proc))

(define-method get-tag-info ((ifd <exif-ifd>)
                             (tag <integer>))
  (get-tag ifd tag (lambda (x) x)))

(define-method get-tag-values ((ifd <exif-ifd>) 
                               (tag <integer>))
  (get-tag ifd tag cdr))

(define-method get-tag-value  ((ifd <exif-ifd>) 
                               (tag <integer>))
  (get-tag ifd tag cadr))

(define-method get-tag-type   ((ifd <exif-ifd>) 
                               (tag <integer>))
  (get-tag ifd tag car))

(define (r->f x) (/ (car x) (cdr x)))

(define-method get-tag-float  ((ifd <exif-ifd>)
                               (tag <integer>))
  (let ((vals (get-tag-info ifd tag)))
    (if vals
        (case (car vals) 
          ((urational srational)
           (r->f (cadr vals)))
          (else
           (errorf "tag(0x~4'0X) is not rational but ~a~%"
                   tag (car vals))))
        vals)))

(define-method get-tag-string ((ifd <exif-ifd>) 
                               (tag <integer>))
  (let ((vals (get-tag-info ifd tag)))
    (if vals
        (if (eq? (car vals) 'ascii-string)
            (apply string (cdr vals))
            (errorf "tag(0x~4'0X) is not ascii-string but ~a~%"
                    tag (car vals)))
        vals)))

;;;
;;;
;;;
(define debug #f)

(define (dmes . args)
  (if debug (apply format args)))

;;;
;;;
;;;
(define (check d s c)
  (let lp  ((s s) (c c))
    (cond ((null? c) #t)
          ((not (= (u8vector-ref d s) (car c))) #f)
          (else
           (lp (+ s 1) (cdr c))))))
  
(define (u8->s  b) (if (> b #x7f)       (- b #xff)       b))
(define (u16->s b) (if (> b #x7fff)     (- b #xffff)     b))
(define (u32->s b) (if (> b #x7fffffff) (- b #xffffffff) b))

(define (get-u8  u8vec pos endian) (u8vector-ref u8vec pos))

(define (get-u16 u8vec pos endian)
  (call-with-input-string (u8vector->string u8vec pos (+ pos 2))
    (lambda (p) (read-binary-uint16 p endian))))

(define (get-u32 u8vec pos endian)
  (call-with-input-string (u8vector->string u8vec pos (+ pos 4))
    (lambda (p) (read-binary-uint32 p endian))))

(define (get-s8 u8vec pos endian)  (u8->s (get-u8 u8vec pos endian)))

(define (get-s16 u8vec pos endian)
  (call-with-input-string (u8vector->string u8vec pos (+ pos 2))
    (lambda (p) (read-binary-sint16 p endian))))
  
(define (get-s32 u8vec pos endian)
  (call-with-input-string (u8vector->string u8vec pos (+ pos 4))
    (lambda (p)  (read-binary-sint32 p endian))))

#|
(call-with-input-string "ABCD"
  (lambda (p)
    (format #t "~4'0X~%" (read-binary-uint16 p 'big-endian))))

(with-input-from-string "ABCDE"
  (lambda ()
    (print (read-binary-uint16))
    (print (read-binary-uint16))
    (print (read-binary-uint16))
    (print (read-binary-uint16))))
|#

;;;
;;;
;;;
(define (app1->tiff-data&endian app1)
  
  (define (MOT-or-INTC mi1 mi2)
    (if (= mi1 mi2) 
        (if (= mi1 (char->integer #\M))
            'MOT
            (if (= mi1 (char->integer #\I))
                'INTC
                (error "invalid TIFF header")))
        (error "invalid TIFF header")))
  
  (if (check app1 0 (map char->integer '(#\E #\x #\i #\f #\null #\null)))
      (let ((mi1 (u8vector-ref app1 6))
            (mi2 (u8vector-ref app1 7)))
        (dmes #t "exif-app1: header check OK!~%")
        (let ((tiff-data (uvector-alias <u8vector>
                                        app1
                                        6
                                        (u8vector-length app1))))
          (case (MOT-or-INTC mi1 mi2)
            ((MOT)   (values tiff-data 'big-endian))
            ((INTC)  (values tiff-data 'little-endian))
            (else
             (error "invalid TIFF header")))))))

(define (root-ifds tiff-data endian)
  (if (= (get-u16 tiff-data 2 endian) #x002a)
      (let ((pos (get-u32 tiff-data 4 endian) ))
        (dmes #t "pos=0x~8'0X~%" pos)
        (IFD-chain tiff-data pos endian '()))
      (error "invalid TIFF header")))

;;;
;;;
;;;
(define (get-subifds tiff-data ifd endian pointer-tag)
  (let ((pos (get-tag-value ifd pointer-tag)))
    (if pos
        (IFD-chain tiff-data pos endian '())
        pos)))

(define (exif-subifds tiff-data ifd0 endian)
  (get-subifds tiff-data ifd0 endian EXIF-IFD-POINTER))

(define (interoperability-subifds tiff-data exif-ifd endian)
  (get-subifds tiff-data exif-ifd endian INTEROPERABILITY-IFD-POINTER))

;;;
;;;
;;;
(define (IFD-chain dat pos endian ifds)

  (define (rd-s8)   (u8->s (read-byte)))
  (define (rd-u16)  (read-binary-uint16 (current-input-port) endian))
  (define (rd-u32)  (read-binary-uint32 (current-input-port) endian))
  (define (rd-urat) (let* ((nume (rd-u32))
                           (deno (rd-u32)))
                      (cons nume deno)))
                                
  (define (rd-s16)  (read-binary-sint16 (current-input-port) endian))
  (define (rd-s32)  (read-binary-sint32 (current-input-port) endian))
  (define (rd-srat) (let* ((nume (rd-s32))
                           (deno (rd-s32)))
                      (cons nume deno)))
  (define (rd-sflo) (read-binary-float  (current-input-port) endian))
  (define (rd-dflo) (read-binary-double (current-input-port) endian))
    
  (define reader-vect             ; ff           len
    (vector #f                    ; 0            NA
            read-byte             ; 1             1
            read-char             ; 2 string      1
            rd-u16                ; 3             2
            rd-u32                ; 4             4
            rd-urat               ; 5             8
            rd-s8                 ; 6             1
            read-byte             ; 7 undefined   1
            rd-s16                ; 8             2
            rd-s32                ; 9             4
            rd-srat               ; 10            8
            rd-sflo               ; 11            4
            rd-dflo               ; 12            8
            ))

  (define (ff->len ff)
    (case ff
      ((1 2 6 7) 1)
      ((3 8)     2)
      ((4 9 11)  4)
      ((5 10 12) 8)
      (else (errorf "invalid IFD data format 0x~4'0X" ff))))

  (define type-vect
    (vector 
     'invalid 
     'uint8   'ascii-string  'uint16   'uint32    'urational  'sint8
     'byte    'sint16        'sint32   'srational 'float      'double
     ))

  (define (ff->type ff)
    (vector-ref type-vect ff))

  (define (rd-dd ff nn ln)
    (cond 
     ((> ln 4)
      (let ((loc (rd-u32)))
        (with-input-from-string
            (u8vector->string dat loc (+ loc ln))
          (lambda () (read-data ff nn)))))
     ((= ln 4)
      (read-data ff nn))
     ((< ln 4)
      (let ((x (read-data ff nn)))
        (do ((i (- 4 ln) (- i 1)))
            ((= i 0) x)
          (read-byte))))))

  (define (read-data ff nn)
    (let ((reader (vector-ref reader-vect ff)))
      (do ((n  nn  (- n 1))
           (l '() (cons (reader) l)))
          ((<= n 0) (reverse l)))))

  (define (rd-ifd n ifd)
    (if (<= n 0)
        #t
        (let* ((tt  (rd-u16))
               (ff  (rd-u16))
               (type (ff->type ff))
               (nn  (rd-u32))
               (ln  (* nn (ff->len ff)))
               (dd  (rd-dd ff nn ln)))
          (dmes #t "tt=0x~4'0X, ff=0x~4'0X, nn=0x~8'0X, ln=~a, dd=~a~%"
                tt ff nn ln dd)
          (if (tag-exists? ifd tt)
              (let ((old (get-tag-info ifd tt)))
                (dmes #t "duplicate tag (0x~4'0X) found: old=~a, new=~a~%"
                      tt old (cons type dd))))
          (put-tag! ifd tt (cons type dd))
          (rd-ifd (- n 1) ifd))))

  (if (= pos 0)
      (reverse ifds)
      (let ((ee  (get-u16 dat pos endian))
            (ifd (make <exif-ifd>)))
        (dmes #t "IFD-chain: pos=0x~4'0X, ee=0x~4'0X~%" pos ee)
        (let ((next (with-input-from-string
                        (u8vector->string dat pos (+ pos (* ee 12)
                                                     4))
                      (lambda () 
                        (rd-u16) ;; ee
                        (rd-ifd ee ifd)
                        (rd-u16) ;; next
                        ))))
          (dmes #t "next=0x~4'0X~%" next)
          (IFD-chain dat next endian (cons ifd ifds))))))

(define (get-app1-from-file file)
  (with-input-from-file file
    (lambda ()
      (let lp ((mk (read-jpeg-marker)))
        (cond ((= (car mk) APP1)
               (cadr mk))
              ((= (car mk) SOS)
               #f)
              (else
               (lp (read-jpeg-marker))))))))

(define (get-tiff-data&endian-from-file file)
  (let ((app1 (get-app1-from-file file)))
    (if (not app1) (errorf "~a is not Exif" file))
    (app1->tiff-data&endian app1)))

(define-syntax use-tags
  (syntax-rules ()
    ((_ sym)
     (define-constant sym (with-module ggc.file.exif sym)))
    ((_ sym1 sym2 ...)
     (begin
       (define-constant sym1 (with-module ggc.file.exif sym1))
       (use-tags sym2 ...)))))
;;;
;;;
;;;
(autoload "ggc/file/exif-data"      
          print-exif-tag ifd-tags  maker-note-getter&tags)

(autoload "ggc/file/exif-fujifilm"  
          maker-note-fujifilm fujifilm-tags)
(autoload "ggc/file/exif-nikon"
          maker-note-nikon  nikon-tags
          maker-note-nikon1 nikon1-tags)
(autoload "ggc/file/exif-olympus"
          maker-note-olympus  olympus-tags)

;;;
;;; XXX: Most of the following APIs will be moved
;;;      to elsewhere.
;;;

;;;
;;;  IFD0
;;;
(define-method get-maker ((ifd0 <exif-ifd>)) 
  (get-tag-string ifd0 MAKE))

(define-method get-model ((ifd0 <exif-ifd>))
  (get-tag-string ifd0 MODEL))

(define-method get-orientaion ((ifd0 <exif-ifd>))
  (case (get-tag-vale ifd0 ORIENTATION)
    ((1) 'top-left)
    ((2) 'top-right)
    ((3) 'bottom-rihgt)
    ((4) 'bottom-left)
    ((5) 'left-top)
    ((6) 'right-top)
    ((7) 'right-bottom)
    ((8) 'left-botom)
    (else #f)))

(define-method get-x-resolution ((ifd0 <exif-ifd>))
  (get-tag-value ifd0 X-RESOLUTION))

(define-method get-y-resolution ((ifd0 <exif-ifd>))
  (get-tag-value ifd0 Y-RESOLUTION))

(define-method get-resolution-unit ((ifd0 <exif-ifd>))
  (get-tag-value ifd0 RESOLUTION-UNIT))

(define-method get-software ((ifd0 <exif-ifd>))
  (get-tag-string ifd0 SOFTWARE))

(define-method get-date-time ((ifd0 <exif-ifd>))
  (get-tag-string ifd0 DATE-TIME))

(define-method get-image-description ((ifd0 <exif-ifd>))
  (get-tag-string ifd0 IMAGE-DESCRIPTION))

(define-method get-copyright ((ifd0 <exif-ifd>))
  (get-tag-string ifd0 COPYRIGHT))

;;;
;;;  EXIF IFD
;;;
(define-method get-exposure-time ((exif <exif-ifd>))
  (get-tag-value exif EXPOSURE-TIME))

(define-method get-f-number ((exif <exif-ifd>))
  (get-tag-float exif F-NUMBER))

(define-method get-exposure-program ((exif <exif-ifd>))
  (case (get-tag-value exif EXPOSURE-PROGRAM)
    ((1) 'manual)
    ((2) 'normal)
    ((3) 'aperture-priority)
    ((4) 'shutter-priority)
    ((5) 'program-creative)
    ((6) 'program-action)
    ((7) 'portrait-mode)
    ((8) 'landscape-mode)
    (else #f)))

(define-method get-iso-speed-rating ((exif <exif-ifd>))
  (get-tag-value exif ISO-SPEED-RATING))

(define-method get-exif-version ((exif <exif-ifd>))
  (let ((vals (get-tag-values exif EXIF-VERSION)))
    (if vals
        (apply string (map integer->char vals))
        vals)))

(define-method get-date-time-original ((exif <exif-ifd>))
  (get-tag-string exif DATE-TIME-ORIGINAL))

(define-method get-components-configuration ((exif <exif-ifd>))

  (define (tran x)
    (case x
      ((1) 'Y)  ((2) 'Cb) ((3) 'Cr)  
      ((4) 'R)  ((5) 'G)  ((6) 'B)
      (else x)))

  (let ((xx (get-tag-values exif COMPONENTS-CONFIGURATION)))
    (if xx (map tran xx) xx)))

(define-method get-compressed-bits-per-pixel ((exif <exif-ifd>))
  (get-tag-float exif COMPRESSED-BITS-PER-PIXEL))

(define-method get-shutter-speed-value ((exif <exif-ifd>))
  (let ((x (get-tag-float exif SHUTTER-SPEED-VALUE)))
    (if x (expt 2 x) x)))

(define-method get-aperture-value ((exif <exif-ifd>))
  (let ((x (get-tag-float exif APERTURE-VALUE)))
    (if x (expt (sqrt 2) x) x)))

(define-method get-brightness-value ((exif <exif-ifd>))
  (get-tag-float exif BRIGHTNESS-VALUE))

(define-method get-Ev ((exif <exif-ifd>))
  (let ((Bv  (get-brightness-value exif))
        (iso (get-iso-speed-rating exif)))
    (and Bv iso (+ Bv
                   (/ (log (/ iso 3.125))
                      (log 2))))))

(define-method get-max-aperture-value ((exif <exif-ifd>))
  (let ((x (get-tag-float exif MAX-APERTURE-VALUE)))
    (if x (expt (sqrt 2) x) x)))

(define-method get-exposure-bias-value ((exif <exif-ifd>))
  (get-tag-value exif EXPOSURE-BIAS-VALUE))

(define-method get-subject-distance ((exif <exif-ifd>))
  (get-tag-float exif SUBJECT-DISTANCE))

(define-method get-metering-mode ((exif <exif-ifd>))
  (case (get-tag-value exif METERING-MODE)
    ((0) 'unknown)
    ((1) 'avarage)
    ((2) 'center-weighted-avarage)
    ((3) 'spot)
    ((4) 'multi-spot)
    ((5) 'multi-segment)
    ((6) 'partial)
    ((255) 'other)
    ((#f) #f)
    (else 'unknown-code)))

(define-method get-light-source ((exif <exif-ifd>))
  (case (get-tag-value exif LIGHT-SOURCE)
    ((0) 'unknown)
    ((1) 'daylight)
    ((2) 'fluorescent)
    ((3) 'tungsten)
    ((10) 'flash)
    ((17) 'standard-light-A)
    ((18) 'standard-light-B)
    ((19) 'standard-light-C)
    ((20) 'D55)
    ((21) 'D65)
    ((22) 'D75)
    ((255) 'other)
    ((#f) #f)
    (else 'unknown-code)))

(define-method get-flash ((exif <exif-ifd>))
  (case (get-tag-value exif FLASH)
    ((0) 'not-fired)
    ((1) 'fired)
    ((5) 'fired-but-strobe-return-light-not-detected)
    ((7) 'fired-and-strobe-return-light-detected)
    ((#f) #f)
    (else 'unknown-code)))

(define-method get-focal-length ((exif <exif-ifd>))
  (get-tag-float exif FOCAL-LENGTH))

(define-method get-flash-pix-version ((exif <exif-ifd>))
  (let ((x (get-tag-values exif FLASH-PIX-VERSION)))
    (if x
        (apply string (map integer->char x))
        x)))

(define-method get-exif-image-width ((exif <exif-ifd>))
  (get-tag-value exif EXIF-IMAGE-WIDTH))

(define-method get-exif-image-height ((exif <exif-ifd>))
  (get-tag-value exif EXIF-IMAGE-HEIGHT))

(define-method get-focal-plane-x-resolution ((exif <exif-ifd>))
  (get-tag-value exif FOCAL-PLANE-X-RESOLUTION))

(define-method get-focal-plane-y-resolution ((exif <exif-ifd>))
  (get-tag-value exif FOCAL-PLANE-Y-RESOLUTION))

(define-method get-focal-plane-resolution-unit ((exif <exif-ifd>))
  (case (get-tag-value exif FOCAL-PLANE-RESOLUTION-UNIT)
    ((1) 'no-unit)
    ((2) 'inch)
    ((3) 'centimeter)
    ((#f) #f)
    (else 'unknown-code)))

(provide "ggc/file/exif")
