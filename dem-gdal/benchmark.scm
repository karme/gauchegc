#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
MOD="$1"
test -z "$MOD" && MOD="dem-gdal"
# |#
:; exec gosh -I. -I../runtime-compile -u$MOD -- $0 "$@"
(use gauche.time)
(use srfi-1)

(define *test-file* "all.vrt")

(define (timed thunk)
  (let1 t (make <real-time-counter>)
    (with-time-counter t
      (thunk))
    (time-counter-value t)))

(define (benchmark count f)
  (let* ((spr (/. (timed
                   (lambda()
                     (dotimes (i count)
                       (f i))))
                  count))
         (rps (/. 1.0 spr)))
    (print rps " calls/s")
    (print spr " s/call")))

(define (main args)
  (let ((z (dem->xy-project->z "epsg:4326" *test-file*))
        (cnt 5000))
    (benchmark cnt (lambda _ (z 8.5 48.5)))
    (let1 test-data
        (list->vector (list-tabulate cnt (lambda _ (list (+ 8.0 (*. (/. (sys-random) RAND_MAX) 2.0))
                                                         (+ 48.0 (/. (sys-random) RAND_MAX))))))
      (benchmark cnt (lambda(i)
                       ;; (let1 p (vector-ref test-data i) #?=(append p (list (apply z p))))
                       (apply z (vector-ref test-data i))
                       ))))
  0)
