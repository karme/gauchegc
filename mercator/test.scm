#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(use mercator)
(use math.const)

(define (dist-mm . l)
  (round->exact
   (* 1000
      (apply great-circle-distance
             (map (^p (map deg->rad p))
                  l)))))

(define (main args)
  (test-start "mercator")
  (test-module 'mercator)
  (test* "great-circle-distance 0°"
         0
         (dist-mm '(0 0) '(0 0)))
  (test* "great-circle-distance 90°"
         #t
         (every (cute = (round->exact (* (* 2 pi earth-sphere-radius 1/4) 1000)) <>)
                (map (cute apply dist-mm <>)
                     '(((90 0) (0 0))
                       ((0 0) (0 90))
                       ((0 0) (0 -90))
                       ((-45 0) (45 0))
                       ((10 0) (100 0))
                       ((0 -10) (0 80))
                       ((0 -45) (0 45))))))
  (test-end)
  0)
