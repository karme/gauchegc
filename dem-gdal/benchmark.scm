#!/bin/bash
#| -*- mode: scheme; coding: utf-8; -*-
USE_RUNTIME_COMPILE="$1"
test -z "$USE_RUNTIME_COMPILE" || USE_RUNTIME_COMPILE="-Fuse-runtime-compile"
# |#
:; exec gosh -I. -I../runtime-compile -I../profile -I../gc-hack -uprofile $USE_RUNTIME_COMPILE -- $0 "$@"

;;(profile-global-hack)

;; disable debug print
(define-syntax debug-print
  (syntax-rules ()
    ((_ ?form)
     ?form)))
(let1 x debug-print 
  (with-module gauche.vm.debugger (set! debug-print x)))

(set! (port-buffering (current-error-port)) :full)

(use dem-gdal)

;;(use dem-gdal-cise)
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
  (let1 cnt 10000
    (let ((z (dem->xy-project->z "epsg:4326" *test-file*)))
      (benchmark cnt (lambda _ (z 8.5 48.5)))
      (let1 test-data
          (list->vector (list-tabulate cnt (lambda _ (list (+ 8.0 (*. (/. (sys-random) RAND_MAX) 2.0))
                                                           (+ 48.0 (/. (sys-random) RAND_MAX))))))
        (benchmark cnt (lambda(i)
                         (apply z (vector-ref test-data i))))))
    (let ((z (dem-stack->xy->z "epsg:4326" '(("N48E008_utm.tif") ("lores.tif")))))
      (benchmark cnt (lambda _ (z 8.5 48.5)))
      (let1 test-data
          (list->vector (list-tabulate cnt (lambda _ (list (+ 8.0 (*. (/. (sys-random) RAND_MAX) 2.0))
                                                           (+ 48.0 (/. (sys-random) RAND_MAX))))))
        (benchmark cnt (lambda(i)
                         (apply z (vector-ref test-data i))))))
    (let ((z (dem-stack->xy->z "epsg:4326" (map (lambda(x)
                                                  (list (string-append
                                                         ;; "/home/karme/mnt/jupiter/var/www/freedem/data/"
                                                         "/vsicurl/http://karme.de/freedem/data/"
                                                         x)))
                                                '("srtm1_west.tif"
                                                  "srtm1_east.tif"
                                                  "srtm3.tif"
                                                  "gmted2010_mn75_fixed.tif"
                                                  "gmted2010_mn30.tif"
                                                  )))))
      (benchmark cnt (lambda _ (z 8.5 48.5)))
      (let1 test-data
          (list->vector (list-tabulate cnt (lambda _ (list (+ 8.0 (*. (/. (sys-random) RAND_MAX) 2.0))
                                                           (+ 48.0 (/. (sys-random) RAND_MAX))))))
        (benchmark cnt (lambda(i)
                         (apply z (vector-ref test-data i))))))
    )
  0)
