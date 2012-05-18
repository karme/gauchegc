#| -*- mode: scheme; coding: utf-8; -*- |#
;;;
;;; google polyline encoder/decoder
;;;
;;; Copyright (C) 2012 Jens Thiele <karme@karme.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; based on:
;;;
;;; PolylineEncoder.js copyright Mark McClure  April/May 2007
;;; http://facstaff.unca.edu/mcmcclur/googlemaps/EncodePolyline/
;;;
;;; This software is placed explicitly in the public
;;; domain and may be freely distributed or modified.
;;; No warranty express or implied is provided.
;;;
;;; todo:
;;; - unit tests
;;; - further improve performance
;;; - support level based decoding

(define-module google-polyline
  (use text.tree)
  (use srfi-1)
  (use gauche.sequence)
  (use sxml.adaptor) ;; for assert macro
  (use gauche.uvector)
  (use runtime-compile)
  (export google-polyline-decode
          google-polyline-decode-levels
          google-polyline-decode-level
          google-polyline-encode
          polyline-dp-exact
          polyline-dp-inexact
          google-polyline-encode-dp-exact
          google-polyline-encode-dp-inexact
          ))

(select-module google-polyline)

(define (decode-next-signed-number get-next)
  (let1 r
      (let loop ((accu 0)
                 (shift 0))
        (let* ((b (get-next))
               (next-accu (logior accu (ash (logand b #x1f) shift))))
          (if (>= b #x20)
            (loop next-accu (+ shift 5))
            next-accu)))
    (if (zero? (logand r 1))
      (ash r -1)
      (lognot (ash r -1)))))

(define (google-polyline-decode e)
  (let ((len (string-size e))
        (index 0)
        (ret '())
        (lat 0)
        (lng 0))
    (let ((get-next (lambda()
                      (let1 r (- (char->integer (ref e index)) 63)
                        (inc! index)
                        r))))
      (while (< index len)
        (inc! lat (decode-next-signed-number get-next))
        (inc! lng (decode-next-signed-number get-next))
        (push! ret (list (/. lng 100000) (/. lat 100000))))) ;; todo: really use /. ?
    (reverse! ret)))

(define (decode-signed-number s)
  (let ((len (string-size s))
        (index 0))
    (decode-next-signed-number (lambda()
                                 (let1 r (- (char->integer (ref s index)) 63)
                                   (inc! index)
                                   r)))))

(define (decode-next-unsigned-number get-next)
  (let loop ((accu 0)
             (shift 0))
    (let* ((b (get-next))
           (next-accu (logior accu (ash (logand b #x1f) shift))))
      (if (>= b #x20)
        (loop next-accu (+ shift 5))
        next-accu))))

(define (decode-unsigned-numbers s)
  (let ((len (string-size s))
        (index 0)
        (ret '()))
    (while (< index len)
      (push! ret (decode-next-unsigned-number (lambda()
                                                (let1 r (- (char->integer (ref s index)) 63)
                                                  (inc! index)
                                                  r)))))
    (reverse! ret)))

;; (decode-unsigned-numbers (apply string-append (map encode-number (iota 18))))

(define google-polyline-decode-levels decode-unsigned-numbers)

(define (google-polyline-decode-level level epl el)
  (filter-map (lambda(p l)
                (if (>= l level)
                  p
                  #f))
              (google-polyline-decode epl)
              (google-polyline-decode-levels el)))

;; (apply google-polyline-decode-level (cons 7 #?=(google-polyline-encode-dp-exact '((10 10) (10.001 10.001) (11 10)))))
;; (apply google-polyline-decode-level (cons 6 #?=(google-polyline-encode-dp-exact '((10 10) (10.001 10.001) (11 10)))))

(define (encode-number n)
  (let loop ((r (list))
             (n n))
    (if (>= n #x20)
      (loop (cons (integer->char (+ (logior #x20 (logand n #x1f)) 63)) r)
            (ash n -5))
      (apply string (reverse! (cons (integer->char (+ n 63))
                                    r))))))

(define (encode-signed-number n)
  (if (< n 0)
    (encode-number (lognot (ash n 1)))
    (encode-number (ash n 1))))

(define (zoom-level-breaks num-levels zoom-factor very-small)
  (map-to <vector> (lambda(i)
                     (* very-small (expt zoom-factor (- num-levels i 1))))
          (iota num-levels)))

(define (level-encoder very-small num-levels zoom-factor)
  (let ((breaks (zoom-level-breaks num-levels
                                   zoom-factor
                                   very-small)))
    
    ;; todo: bug: what about dd < verysmall?!
    (define (compute-level dd)
      (if (> dd very-small)
        (let loop ((lev 0))
          (if (< dd (vector-ref breaks lev))
            (loop (+ lev 1))
            lev))
        (assert #f)))
    
    (lambda(points dists)
      (tree->string
       (let1 s-1 (- (size-of points) 1)
         (map-with-index (lambda(i p d)
                           (cond [(or (= i 0) (= i s-1))
                                  (encode-number (- num-levels 1))]
                                 [(undefined? d)
                                  ""]
                                 [else
                                  (encode-number (- num-levels (compute-level d) 1))]))
                         points
                         dists))))))

(define (point-encoder)
  (let ((pp (list 0 0)))
    (lambda(p)
      (let1 np (list (floor->exact (* (car p) 100000))
                     (floor->exact (* (cadr p) 100000)))
        (let1 r (string-append (encode-signed-number (- (cadr np) (cadr pp)))
                               (encode-signed-number (- (car np)  (car pp))))
          (set! pp np)
          r)))))

(define (google-polyline-encode points)
  (tree->string
   (let1 enc (point-encoder)
     (map enc points))))

(define-syntax m-list-2d-distance²
  (syntax-rules()
    [(m-list-2d-distance² a b)
     (let ((la a)
           (lb b))
       (let ((x (- (car lb)  (car la)))
             (y (- (cadr lb) (cadr la))))
         (+ (* x x) (* y y))))]))

(define-syntax m-list-2d-line-segment-point-rel-dist²
  (syntax-rules()
    [(m-list-2d-line-segment-point-rel-dist² a b)
     (let ((la a)
           (lb b))
       ;; (assert (not (equal? la lb)))
       (let ((abx (- (car  lb) (car  la)))
             (aby (- (cadr lb) (cadr la))))
         (let* ((l² (+ (* abx abx) (* aby aby))))
           (lambda(p)
             (let ((apx (- (car  p) (car  la)))
                   (apy (- (cadr p) (cadr la))))
               (let ((t (+ (* apx abx) (* apy aby)))
                     (d (- (* apx aby) (* apy abx))))
                 (cond [(< t 0)
                        (+ (* d d) (* t t))]
                       [(> t l²)
                        (let1 t (- t l²)
                          (+ (* d d) (* t t)))]
                       [else
                        (* d d)])))))))]))

(define (polyline-dp-exact points very-small)
  (assert (vector? points))
  (let ((dists (make-vector (size-of points))))
    (when (> (size-of points) 2)
      (let loop ((from 0)
                 (to (- (size-of points) 1)))
        ;; find point in (subseq points (+ from 1) (- to 1)) farthest away
        ;; todo: improve
        (let ((maxDist 0)
              (maxLoc -1))
          ;; todo: we should calculate distance in screen coordinates
          ;; => we should project to epsg:3857 first
          (let1 segment-rel-distance²
              (if (equal? (vector-ref points from) (vector-ref points to))
                (cut m-list-2d-distance² (vector-ref points from) <>)
                (m-list-2d-line-segment-point-rel-dist² (vector-ref points from)
                                                        (vector-ref points to)))
            (do [(i (+ from 1) (+ i 1))]
                [(>= i to)]
              (let1 temp (segment-rel-distance² (vector-ref points i))
                (when (> temp maxDist)
                  (set! maxDist temp)
                  (set! maxLoc i)))))
          (let1 maxDist (sqrt (if (equal? (vector-ref points from) (vector-ref points to))
                                maxDist
                                (/ maxDist (m-list-2d-distance² (vector-ref points from)
                                                                (vector-ref points to)))))
            (when (> maxDist very-small)
              ;; (assert (undefined? (ref dists maxLoc)))
              (set! (vector-ref dists maxLoc) maxDist)
              (loop from maxLoc)
              (loop maxLoc to))))))
    dists))

(compile-and-load
 `((inline-stub
    (define-cproc cline-rel-dist2 (l::<f64vector> px::<double> py::<double>)
      ::<number> ;; :constant (constant not available in gauche 0.9)
      (let* ((ax::double  (SCM_F64VECTOR_ELEMENT l 0))
             (ay::double  (SCM_F64VECTOR_ELEMENT l 1))
             (abx::double (SCM_F64VECTOR_ELEMENT l 2))
             (aby::double (SCM_F64VECTOR_ELEMENT l 3))
             (ll::double  (SCM_F64VECTOR_ELEMENT l 4))
             (apx::double (- px ax))
             (apy::double (- py ay))
             (t::double (+ (* apx abx) (* apy aby)))
             (d::double (- (* apx aby) (* apy abx))))
        (if (< t 0)
          (result (Scm_MakeFlonum (+ (* d d) (* t t))))
          (if (> t ll)
            (let* ((x::double (- t ll)))
              (result (Scm_MakeFlonum (+ (* d d) (* x x)))))
            (result (Scm_MakeFlonum (* d d)))))))
    ))
 '(cline-rel-dist2))

(define-syntax m-list-2d-line-segment-point-rel-dist²-inexact
  (syntax-rules()
    [(m-list-2d-line-segment-point-rel-dist²-inexact a b)
     (let ((la a)
           (lb b))
       ;; (assert (not (equal? la lb)))
       (let ((abx (- (car  lb) (car  la)))
             (aby (- (cadr lb) (cadr la))))
         (let* ((l² (+ (* abx abx) (* aby aby)))
                (v (f64vector (car la) (cadr la) abx aby l²)))
           (lambda(p)
             (cline-rel-dist2 v (car p) (cadr p))))))]))

;; todo: nearly the same as polyline-dp-exact
(define (polyline-dp-inexact points very-small)
  (assert (vector? points))
  (let ((dists (make-vector (size-of points))))
    (when (> (size-of points) 2)
      (let loop ((from 0)
                 (to (- (size-of points) 1)))
        ;; find point in (subseq points (+ from 1) (- to 1)) farthest away
        ;; todo: improve
        (let ((maxDist 0)
              (maxLoc -1))
          ;; todo: we should calculate distance in screen coordinates
          ;; => we should project to epsg:3857 first
          (let1 segment-rel-distance²
              (if (equal? (vector-ref points from) (vector-ref points to))
                (cut m-list-2d-distance² (vector-ref points from) <>)
                (m-list-2d-line-segment-point-rel-dist²-inexact (vector-ref points from)
                                                                (vector-ref points to)))
            (do [(i (+ from 1) (+ i 1))]
                [(>= i to)]
              (let1 temp (segment-rel-distance² (vector-ref points i))
                (when (> temp maxDist)
                  (set! maxDist temp)
                  (set! maxLoc i)))))
          (let1 maxDist (sqrt (if (equal? (vector-ref points from) (vector-ref points to))
                                maxDist
                                (/ maxDist (m-list-2d-distance² (vector-ref points from)
                                                                (vector-ref points to)))))
            (when (> maxDist very-small)
              ;; (assert (undefined? (ref dists maxLoc)))
              (set! (vector-ref dists maxLoc) maxDist)
              (loop from maxLoc)
              (loop maxLoc to))))))
    dists))

(define (google-polyline-encode-dp-2 points dp . args)
  (let-optionals* args ((very-small 0.00001)
                        (num-levels 18)
                        (zoom-factor 2))
    
    (define (encode-points points dists)
      (tree->string
       (let ((enc (point-encoder))
             (s-1 ( - (size-of points) 1)))
         (map-with-index (lambda(i p d)
                           (if (or (not (undefined? d))
                                   (= i 0)
                                   (= i s-1))
                             (enc p)
                             ""))
                         points
                         dists))))
    (let ((dists (dp (coerce-to <vector> points) very-small))
          (encode-levels (level-encoder very-small num-levels zoom-factor)))
      (list (encode-points points dists)
            (encode-levels points dists)))))

(define google-polyline-encode-dp-exact
  (cut google-polyline-encode-dp-2 <> polyline-dp-exact))

(define google-polyline-encode-dp-inexact
  (cut google-polyline-encode-dp-2 <> polyline-dp-inexact))
