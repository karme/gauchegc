#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use svg-plot)
(use file.util)
(use gauche.process)

(define (main args)
  (with-output-to-process
   `(see ,#`"image/svg+xml:-")
   (cut svg-plot
        '(((0 0) (10 10) (20 5) (30 30))
          ((0 0) (10 9) (20 4) (30 20)))))
  (with-output-to-process
   `(see ,#`"image/svg+xml:-")
   (cut svg-plot
        '(((0 0) (10 10) (20 5) (30 30))
          ((0 0) (10 9) (20 4) (30 20)))
        '("test1" "test2")))
  (with-output-to-process
   `(see ,#`"image/svg+xml:-")
   (cut svg-plot-3d
        '((((0 0 0.0) (1 0 0.0) (2 0  0.0))
           ((0 1 0.0) (1 1 0.6) (2 1  0.0))
           ((0 2 0.0) (1 2 0.0) (2 2  1.5)))
          (((0 0 0.2) (1 0 0.0) (2 0 -0.5))
           ((0 1 0.0) (1 1 0.4) (2 1  0.0))
           ((0 2 0.0) (1 2 0.0) (2 2  0.3))))
        '("test1" "test")))
  0)
