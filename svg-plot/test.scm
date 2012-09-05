#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use svg-plot)
(use file.util)
(use gauche.process)

(define (main args)
  (with-output-to-process
   `(see ,#`"image/svg+xml:-")
   (cut svg-plot '((0 0) (10 10) (20 5) (30 30)) "a test"))
  0)
