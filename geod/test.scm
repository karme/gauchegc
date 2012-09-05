#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../runtime-compile -- $0 "$@"

(use gauche.sequence)
(use srfi-1)
(use gauche.test)
(use sxml.adaptor) ;; for assert
(test-start "geod")
(use geod)
(test-module 'geod)

(define (small-error? expect got)
  (< (abs (- expect got)) 1.0e-9))

(test "geod-direct and geod-inverse"
      '(10 100000)
      (lambda()
        (let* ((p1 (list 9 48.5))
               (p2 (geod-direct 'wgs84 p1 10 100000)))
          (geod-inverse 'wgs84 p1 p2)))
      (lambda(expected result)
        (every boolean (map small-error?
                            expected result))))

(test* "geod-upsample-polyline"
       #t
       (let* ((p1 (list 9 48.5))
              (p2 (geod-direct 'wgs84 p1 10 100000))
              (max-dist 50)
              (pl (geod-add-measure 'wgs84
                                    (geod-upsample-polyline 'wgs84
                                                            (list p1 p2)
                                                            max-dist))))
         (fold (lambda(n o)
                 (assert (small-error? (geod-distance 'wgs84 o n) (- (ref n 2) (ref o 2))))
                 (assert (< (- (ref n 2) (ref o 2) 1.0e-8) max-dist)) ;; todo: do we really accept that error?
                 n)
               (car pl)
               (cdr pl))
         #t))

(test-end)
