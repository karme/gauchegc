;;;
;;; Maker note information for Fujifilm.
;;;
(select-module ggc.file.exif)
;;;
;;; Fujifilm
;;; 
(define (maker-note-fujifilm tiff-data exif-ifd endian)
  (let ((dl (get-tag-values exif-ifd MAKER-NOTE)))
    (if dl
        (let ((data (apply u8vector dl)))
          (if (check data 0 (map char->integer 
                                 '(#\F #\U #\J #\I #\F #\I #\L #\M)))
              ;;; actual data starts at 8 with ``ee'',.
              ;;; which is entry offset of IFD, and 
              ;;; is always 12.
              (IFD-chain data 12 'little-endian '())
              (error "Invalid FUJIFILM maker note")))
        dl)))

(define fujifilm-tags (make-hash-table))
(for-each (lambda (x)
            (hash-table-put! fujifilm-tags (car x) (cdr x)))
          '(
            (#x0000 version        byte)
            (#x1000 quality        ascii-string)
            (#x1001 sharpness      uint16)
            (#x1002 white-balance  uint16)
            (#x1003 color          uint16)
            (#x1004 tone           uint16)
            (#x1010 flash-mode     uint16)
            (#x1011 flash-strengh  urational)
            (#x1020 macro          uint16)
            (#x1021 focus-mode     uint16)
            (#x1030 slow-sync      uint16)
            (#x1031 picture-mode   uint16)
            (#x1032 unknown-0x1032 uint16)
            (#x1100 cont-take/bracket uint16)
            (#x1200 unknown-0x1200 uint16)
            (#x1300 blur-warning   uint16)
            (#x1301 focus-warning  uint16)
            (#x1302 ae-warning     uint16)
            ))

; EOF
