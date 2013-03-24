#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
exec gosh -I. -- $0 "$@"
|#
(use gps)
(use gauche.selector)

(define (main args)
  (let ((selector (make <selector>))
        (selectable (gps-open)))
    (selector-add! selector
                   selectable
                   ;; (gps-tpv-handler print)
                   (gps-position-handler (lambda(dev lon lat alt)
                                           (print dev " " lon " " lat " " alt)))
                   '(r))
    (do () (#f) (selector-select selector))))
