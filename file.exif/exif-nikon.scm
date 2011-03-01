;;;
;;; Maker note information for Nikon.
;;;
(select-module ggc.file.exif)
;;;
;;; Nikon E700/E800/E900/E900S/E910/E950
;;; 
(define (maker-note-nikon tiff-data exif-ifd endian)
  (let ((pos (get-maker-note-offset tiff-data endian)))
    (if (check tiff-data pos (map char->integer '(#\N #\i #\k #\o #\n #\null)))
        (IFD-chain tiff-data (+ pos 8) endian '())
        (error "Invalid Nikon maker-note"))))

;;;
;;; Nikon E990/D1, Casio.
;;;
(define (maker-note-nikon1 tiff-data exif-ifd endian)
  (let ((pos (get-maker-note-offset tiff-data endian)))
    (IFD-chain tiff-data pos endian '())))

;;;
;;; TAG description table
;;; 
(define nikon-tags (make-hash-table))
(for-each (lambda (x)
            (hash-table-put! nikon-tags (car x) (cdr x))) 
         '(
            (#x0002 unknown          ascii-string)
            (#x0003 quality          uint16)
            (#x0004 color-mode       uint16)
            (#x0005 image-adjustment uint16)
            (#x0006 CCD-sensitivity  uint16)
            (#x0007 white-balance    uint16)
            (#x0008 focus            urational)
            (#x0009 unknown          ascii-string)
            (#x000a digital-zoom     urational)
            (#x000b converter        uint16)
            (#x0f00 unknown          uint32)
            ))
  
(define nikon1-tags (make-hash-table))
(for-each (lambda (x)
            (hash-table-put! nikon1-tags (car x) (cdr x)))
          '(
            (#x0001 unknown          byte)
            (#x0003 color-mode       ascii-string)
            (#x0004 quality          ascii-string)
            (#x0005 white-balance    ascii-string)
            (#x0006 image-sharpening ascii-string)
            (#x0007 focus-mode       ascii-string)
            (#x0008 flash-setting    ascii-string)
            (#x000a unknown          urational)
            (#x000f iso-selection    ascii-string)
            (#x0080 image-adustment  ascii-string)
            (#x0082 adapter          ascii-string)
            (#x0085 manual-forcus-distance urational)
            (#x0086 digital-zoom     urational)
            (#x0088 AF-focus-position byte)
            (#x0010 data-dump        byte)
            ))
; EOF
