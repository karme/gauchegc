;;;
;;; dem (digital elevation model) via gdal (http://www.gdal.org)
;;;
;;;   Copyright (c) 2012 Jens Thiele <karme@karme.de>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; notes/todo:
;; - quite a hack
;; - get rid of c-wrapper / speedup
;; - leaks memory => call procedures only once if possible!
;; - you can use gdal's vrt format to merge images
;;   see also: http://www.gdal.org/gdal_vrttut.html
;;   and in general (WMS,....)
;;   http://www.gdal.org/formats_list.html
;; - a more general purpose gdal wrapper would be nice
;;   (upload to gl texture ...)
;; - use GDAL_CACHEMAX?
(define-module dem-gdal
  (use srfi-1)
  (use gauche.collection) ;; use after srfi-1 to make find work as expected!
  (use srfi-13)
  (use c-wrapper)
  (use gauche.array)
  (use gauche.uvector)
  (use gauche.process)
  (export dem->xy->z   
          dem->xy-project->z
          dem-range-error?  ;; outside of dataset area => range error
          dem-nodata-error? ;; inside of dataset but no data at that point => nodata error (is a range error, too)
          ))

(select-module dem-gdal)

(c-load '("gdal/gdal.h" "gdal/ogr_srs_api.h") :libs-cmd "gdal-config --libs")

(define-macro (assert e)
  `(when (not ,e)
     (error "assertion failed: " ,(x->string e))))

(define (gdal-open-dataset name)
  (with-output-to-port (current-error-port)
    (lambda()
      (gdal-init)
      (let ((dataset (GDALOpen name GA_ReadOnly)))
        (cond [(not (null-ptr? dataset))
               (let ((driver (GDALGetDatasetDriver dataset)))
                 ;; (print #`"Driver ,(GDALGetDriverShortName driver)/,(GDALGetDriverLongName driver)")
                 ;; (print #`"Size is ,(GDALGetRasterXSize dataset)x,(GDALGetRasterYSize dataset)x,(GDALGetRasterCount dataset)")
                 (when (not (null-ptr? (GDALGetProjectionRef dataset)))
                   ;; (print #`"Projection is ',(GDALGetProjectionRef dataset)'")
                   (let ((transform (make (c-array <c-double> 6))))
                     (when (= (GDALGetGeoTransform dataset transform) CE_None)
                       ;;#?=(map (cut cast <number> <>) transform)
                       ;; (print #`"Origin = ,(ref transform 0), ,(ref transform 3)")
                       ;; (print #`"Pixel Size = ,(ref transform 1), ,(ref transform 5)")
                       ))))
               dataset]
              [else
               (error "Unsupported format")])))))

;; (define (osr-from-epsg epsg)
;;   (assert (number? epsg))
;;   (assert (exact? epsg))
;;   (let ((hSRS (OSRNewSpatialReference NULL))) ;; todo: leak!
;;     (when (not (= (OSRImportFromEPSG hSRS epsg) OGRERR_NONE))
;;       (error "OSRImportFromEPSG failed"))
;;     hSRS))

;; (define (osr-from-proj4 proj4)
;;   (let ((hSRS (OSRNewSpatialReference NULL))) ;; todo: leak!
;;     (when (not (= (OSRImportFromProj4 hSRS proj4) OGRERR_NONE))
;;       (error "OSRImportFromProj4 failed"))
;;     hSRS))

;; (define (c-string s)
;;   ;; todo: this only works with single byte chars!
;;   ;; see: (c-string "ä")
;;   (let ((ret (cast (c-array <c-char> (+ (size-of s) 1)) (cast <c-ptr> s))))
;;     (assert (= (cast <integer> (ref ret (- (size-of ret) 1))) 0))
;;     ret))
;; (define (osr-from-wkt s)
;;   (let ((hSRS (OSRNewSpatialReference NULL)) ;; todo: leak!
;;         (cs (c-string s)))
;;     (when (not (= (OSRImportFromWkt hSRS #?=(ptr cs)) OGRERR_NONE))
;;       (error "OSRImportFromWKT failed"))
;;     hSRS))

(define (osr-from-user-input s)
  (let ((hSRS (OSRNewSpatialReference NULL))) ;; todo: leak!
    (when (not (= (OSRSetFromUserInput hSRS s) OGRERR_NONE))
      (error "OSRImportFromWKT failed"))
    hSRS))

(define (osr-from-dataset dataset)
  (let ((hSRS (OSRNewSpatialReference NULL)))
    (OSRImportFromWkt hSRS (ptr (GDALGetProjectionRef dataset)))
    hSRS))

(define (osr-transform from to)
  (let ((ct (OCTNewCoordinateTransformation from to))
        (xa (make (c-array <c-double> 1)))
        (ya (make (c-array <c-double> 1)))
        (za (make (c-array <c-double> 1))))
    (assert (not (null-ptr? ct)))
    (lambda(l)
      (set! (ref xa 0) (ref l 0))
      (set! (ref ya 0) (ref l 1))
      (set! (ref za 0) (ref l 2 0))
      (OCTTransform ct 1 xa ya za)
      (list (ref xa 0) (ref ya 0) (ref za 0)))))

(define (gdal-get-projection dataset)
  (let ((hSRS (osr-from-dataset dataset)))
    (if (OSRIsProjected hSRS)
      (osr-transform (OSRCloneGeogCS hSRS) hSRS)
      identity))) ;; (lambda(l) l))))

(define (gdal-get-geotransform-matrix dataset)
  (let ((m (make (c-array <c-double> 6))))
    (GDALGetGeoTransform dataset (ptr m))
    (apply array (cons (shape 0 3 0 3)
                       (append (map (cut ref m <>) '(1 2 0))
                               (map (cut ref m <>) '(4 5 3))
                               '(0.0 0.0 1.0))))))

;; todo:
;; - gdal already should provide that, no?
;; - slow
(define (gdal-get-projected-cs-to-raster-cs dataset)
  (let ((A (array-inverse (array-mul (gdal-get-geotransform-matrix dataset)
                                     (array (shape 0 3 0 3)
                                            1.0 0.0 0.5
                                            0.0 1.0 0.5
                                            0.0 0.0 1.0)))))
    (lambda(l)
      (let1 r (array-mul A (array (shape 0 3 0 1) (ref l 0) (ref l 1) 1))
        (list (array-ref r 0 0) (array-ref r 1 0))))))

(define (c-int->bool x)
  (not (= 0 (cast <number> x))))

(define (gdal-open-band dataset band)
  (let ((hband (GDALGetRasterBand dataset band))
        ;; (block-size-x (make <c-int>))
        ;; (block-size-y (make <c-int>))
        ;; (gotMin (make <c-int>))
        ;; (gotMax (make <c-int>))
        ;; (adfMinMax (make (c-array <c-double> 2)))
        )
    ;; (GDALGetBlockSize hband (ptr block-size-x) (ptr block-size-y))
    ;; (print #`"Block=,(cast <number> block-size-x)x,(cast <number> block-size-y) Type=,(GDALGetDataTypeName (GDALGetRasterDataType hband)), ColorInterp=,(GDALGetColorInterpretationName (GDALGetRasterColorInterpretation hband))")
    ;; (set! (ref adfMinMax 0) (GDALGetRasterMinimum hband (ptr gotMin)))
    ;; (set! (ref adfMinMax 1) (GDALGetRasterMaximum hband (ptr gotMax)))
    ;; (when (not (and (c-int->bool gotMin) (c-int->bool gotMax)))
    ;;   (GDALComputeRasterMinMax hband TRUE adfMinMax))
    ;;        (print #`"Min=,(ref adfMinMax 0), Max=,(ref adfMinMax 1)")
    ;; (when (< 0 (GDALGetOverviewCount hband))
    ;;   (print "Band has ,(GDALGetOverviewCount hband) overviews."))
    ;; (when (not (null-ptr? (GDALGetRasterColorTable hband)))
    ;;   (print #`"Band has a color table with ,(GDALGetColorEntryCount (GDALGetRasterColorTable hband)) entries."))
    hband))

(define (gdal-band-nodata hband)
  (let ((gotNoData (make <c-int>)))
    (GDALGetRasterNoDataValue hband (ptr gotNoData))
    (if (c-int->bool gotNoData)
      (GDALGetRasterNoDataValue hband (ptr gotNoData))
      #f)))

(define-condition-type <dem-range-error> <error>
  dem-range-error?)

(define-condition-type <dem-nodata-error> <dem-range-error>
  dem-nodata-error?)

(define (gdal-read-band-row band row . args)
  (let-optionals* args ((start 0)
                        (end (GDALGetRasterBandXSize band)))
    (when (not (and (>= start 0)
                    (> end start)
                    (<= end (GDALGetRasterBandXSize band))))
      (error <dem-range-error>))
    (when (not (and (>= row 0)
		    (< row (GDALGetRasterBandYSize band))))
      (error <dem-range-error>))
    (let* ((count (- end start))
           (scanline (make-f32vector count)))
      (when (not (zero? (GDALRasterIO band GF_Read start row count 1 scanline count 1 GDT_Float32 0 0)))
        (error <dem-range-error>))
      scanline)))

;; taken from grass (interp.c)
;;     return (u * (u * (u * (c3 + -3 * c2 + 3 * c1 - c0) +
;;	      (-c3 + 4 * c2 - 5 * c1 + 2 * c0) + (c2 - c0)) + 2 * c1) / 2;
(define (interp-cubic u c0 c1 c2 c3)
  (/ (+ (* u (+ (* u (+ (* u (+ c3 (* -3 c2) (* 3 c1) (- c0)))
                        (- c3)
                        (* 4 c2)
                        (* -5 c1)
                        (* 2 c0)))
                c2
                (- c0)))
        (* 2 c1))
     2))

(define (interp-linear u c0 c1)
  (+ (* u (- c1 c0)) c0))

(define (bi-interp u v f rows)
  (apply f
         (cons v
               (map (lambda(x)
                      (apply f
                             (cons u
                                   (coerce-to <list> (ref rows x)))))
                    (iota (size-of rows))))))

(define (interp-bicubic u v rows)
  (assert (= (size-of rows) 4))
  (bi-interp u v interp-cubic rows))

(define (interp-bilinear u v rows)
  (assert (= (size-of rows) 2))
  (bi-interp u v interp-linear rows))

(define (gdal-read-band-4x4 read-band-row raster-pos)
  (let ((uv (map (lambda(x) (- x (floor x))) raster-pos))
        (tl (map (compose (cut + <> -1) floor->exact) raster-pos)))
    (append uv
            (list
             (map
              (lambda(y) (read-band-row (+ (ref tl 1) y)
                                        (ref tl 0)
                                        (+ (ref tl 0) 4)))
              '(0 1 2 3))))))

(define (gdal-read-band-2x2 read-band-row raster-pos)
  (let ((uv (map (lambda(x) (- x (floor x))) raster-pos))
        (tl (map floor->exact raster-pos)))
    (append uv
            (list
             (map
              (lambda(y) (read-band-row (+ (ref tl 1) y)
                                        (ref tl 0)
                                        (+ (ref tl 0) 2)))
              '(0 1))))))

(define (gdal-read-band-pixel read-band-row raster-pos)
  (let1 p (map round->exact raster-pos)
    (f32vector-ref (read-band-row (cadr p) (car p) (+ (car p) 1)) 0)))

(define gdal-init
  (let1 called #f
    (lambda()
      (cond [(not called)
             (set! called #t)
             (GDALAllRegister)
             #t]
            [else
             #f]))))

(define (nan-on-dem-range-error proc)
  (lambda l
    (guard (e
            [(dem-range-error? e)
             +nan.0]
            [else
             (raise e)])
           (apply proc l))))

(define (error-on-nodata proc nodata)
  (lambda l
    (let1 r (apply proc l)
      (when (find (cut = nodata <>) r)
        (error <dem-nodata-error>))
      r)))

;; return function to get z value at position x y (using coordinate system described by projection)
(define (dem->xy-project->z projection name . args)
  (let-optionals* args ((use-nan? #t) ;; use nan to represent nodata or throw error?
			(interpolation 'bi-cubic)
                        (band 1))
    (let* ((dataset (gdal-open-dataset name))
           (band (gdal-open-band dataset band))
           (compose-xy->z (lambda(fi fg)
                            (let1 p ((if use-nan?
                                       nan-on-dem-range-error
                                       (lambda(proc) proc))
                                     (apply compose
                                            (reverse
                                             (append
                                              (if (not (string-null? projection))
                                                (list (osr-transform (osr-from-user-input projection)
                                                                     (OSRCloneGeogCS (osr-from-dataset dataset))))
                                                (list))
                                              (list (gdal-get-projection dataset)
                                                    (gdal-get-projected-cs-to-raster-cs dataset)
                                                    (cut fg
                                                         (let1 read-row (cut gdal-read-band-row band <...>)
                                                           (if (gdal-band-nodata band)
                                                             (error-on-nodata read-row (gdal-band-nodata band))
                                                             read-row))
                                                         <...>)
                                                    (cut apply fi <>))))))
                              (lambda(x y)
                                (p (list x y)))))))
      (case interpolation
        ((bi-cubic)  (compose-xy->z interp-bicubic gdal-read-band-4x4))
        ((bi-linear) (compose-xy->z interp-bilinear gdal-read-band-2x2))
        ((nearest)   (compose-xy->z identity (compose list gdal-read-band-pixel)))
        (else (error "Unknown interpolation:" interpolation))))))

;; return function to get z value at position x y (using coordinate system of the dataset)
(define (dem->xy->z name . args)
  (apply dem->xy-project->z (append (list "" name) args)))
