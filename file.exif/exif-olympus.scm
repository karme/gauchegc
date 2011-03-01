;;;
;;; Maker note information for OLYMPUS.
;;;
(select-module ggc.file.exif)
;;;
;;;
;;;
(define (maker-note-olympus tiff-data exif-ifd endian)
  (let ((pos (get-maker-note-offset tiff-data endian)))
    (if (check tiff-data pos (map char->integer '(#\O #\L #\Y #\M #\P #\null)))
        (IFD-chain tiff-data (+ pos 8) endian '())
        (error "Is this really olympus?"))))

(define olympus-tags (make-hash-table))
(for-each (lambda (x)
            (hash-table-put! olympus-tags (car x) (cdr x)))
          '(
            (#x0200 special-mode uint32)
            (#x0201 jpeg-qual    uint16)
            (#x0202 macro        uint16)
            (#x0203 unknown      uint16)
            (#x0204 digital-zoom urational)
            (#x0205 unknown      urational)
            (#x0206 unknown      uint16)
            (#x0207 software-release ascii-string)
            (#x0208 pict-info    ascii-string)
            (#x0209 camera-id    byte)
            (#x0f00 data-dump    uint32)
            ))
