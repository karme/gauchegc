#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I../runtime-compile -I. -- $0 "$@"
(use xml2sxml)
(use sxml.adaptor) ;; for assert

(define (main args)
  (let* ((reader (make-xml-reader (current-input-port)))
         (handle-node (lambda()
                        (let1 expr (xml-reader-node reader)
                          (when expr
                            (write expr)
                            (newline))))))
    (assert (and (xml-reader-read reader)
                 (xml-reader-read reader)))
    (handle-node)
    (while (xml-reader-next reader)
      (handle-node))
    (close-xml-reader reader))
  0)
