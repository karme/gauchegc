#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(use rfc.http)
(use gauche.process)
(use srfi-1)
(use gauche.collection)

(test-start "dem-gdal")
(use dem-gdal)
(test-module 'dem-gdal)

(define *test-files* '("N48E008.hgt" "N48E009.hgt"))
(define *vrt-file* "all.vrt")

(define (download host path dest)
  (call-with-output-file dest
    (lambda (out)
      (http-get host path :sink out :flusher (lambda _ #t)))))

(define (unzip x)
  (run-process `(unzip ,x) :wait #t)
  (sys-unlink x))

;;; download and unzip data files if needed

(for-each
 (lambda(file)
   (when (not (file-exists? file))
     (with-output-to-port (current-error-port)
       (cut print "downloading " file))
     (download "dds.cr.usgs.gov"
               (string-append "/srtm/version2_1/SRTM3/Eurasia/"
                              file
                              ".zip")
               (string-append file ".zip"))
     (unzip (string-append file ".zip"))))
 *test-files*)

;;; build test vrt file

(when (not (file-exists? *vrt-file*))
  (with-output-to-port (current-error-port)
    (cut print "build vrt file " *vrt-file*))
  (run-process (append `(gdalbuildvrt ,|*vrt-file*|)
                       *test-files*)
               :wait #t))

;; (run-process `(gdaldem hillshade -s 111120 ,|*vrt-file*| hillshade.tif) :wait #t)

;;; the real tests

;; make sure find works as expected on collections
;; see also gauche mailing list:
;; Message-ID: <87d34i9smp.fsf@karme.de>
(test* "find"
       -10000.0
       (find (cut = -10000.0 <>) #f32(-10000.0 -10000.0)))

(test* "dem->xy->z"
       735
       (let1 z (dem->xy->z *vrt-file*)
         (round->exact (z 8.5 48.5))))

(test* "dem->xy-project->z"
       735
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*)
         (round->exact (z 8.5 48.5))))

(test* "dem->xy-project->z"
       735
       (let1 z (dem->xy-project->z "epsg:25832" *vrt-file*)
         (round->exact (z 463064.1314 5371996.2822))))

(test* "dem->xy-project->z nodata(1)?"
       #f
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*)
         (nan? (z 8.5 48.5))))

(test* "dem->xy-project->z nodata(2)?"
       #t
       (let1 z (dem->xy-project->z "epsg:4326" *vrt-file*)
         (nan? (z 8.29151 48.87847))))

(test* "dem->xy-project->z test error"
       #t
       (guard (e [(dem-range-error? e)
                  #t])
              (let1 z (dem->xy-project->z "epsg:4326" *vrt-file* #f)
                (z 8.29151 48.87847))))

(test-end :exit-on-failure #t)
