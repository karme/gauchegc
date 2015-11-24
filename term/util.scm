(define-module ggc.term.util
  (use gauche.process)
  (export-all)
)
(select-module ggc.term.util)

;;;
;;; NOTE: resize usually resides /usr/X11/bin
;;;
(define (get-terminal-size)
  (with-input-from-process "resize -u"
    (lambda ()
      (let ((x (cadr (string-split (read-line) #\=)))
            (y (cadr (string-split (read-line) #\=))))
        (let ((xx (string->number (substring x 0 (- (string-length x) 1))))
              (yy (string->number (substring y 0 (- (string-length y) 1)))))
          (values xx yy))))))

(provide "ggc/term/with-raw-mode")
