(define-module ggc.file.util
  (export basename)
)
(select-module ggc.file.util)
;;;
;;;
;;;
(define (basename name suffix)
  (let ((name (sys-basename name)))
    (let ((bn (rxmatch (string->regexp (string-append suffix "$"))
		       name)))
      (if bn 
          (rxmatch-before bn)
          name))))

(provide "ggc/file/util")
