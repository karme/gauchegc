;;;
;;; simple statistics
;;;
;;;   Copyright (c) 2015 Jens Thiele <karme@karme.de>
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
;;; todo: maybe just port http://docs.racket-lang.org/math/stats.html
;;;
;;; note/remember: also did write more complex generator versions
;;; (median was ok, but quantile was still missing)
;;;
;;; see also: gnu R help(fivenum), help(quantile), help(sd), help(mean)

(define-module math.statistics
  (export five-number-summary
          mean
          quantile
          median
          stddev
          stddev-bessel-corrected))

(select-module math.statistics)

;; todo: allow to trim? weighted-mean? other means?
;; s.a. https://en.wikipedia.org/wiki/Mean
(define (mean l) (/ (apply + l) (length l)))

;; note: type 2 aka R-2 SAS-5 Maple-2
;; todo: allow to pass cmpfn?
(define (quantile p l)
  (let* ((l (sort l))
         (cnt (length l))
         (pos (* p cnt)))
    (if (integer? pos)
      (let1 l (drop l (- pos 1))
        (mean (list (car l) (cadr l))))
      (list-ref l (floor pos)))))

(define median (cut quantile 1/2 <>))

;; https://en.wikipedia.org/wiki/Five-number_summary
;; todo: api: return list instead of values? allow to pass cmpfn?
(define (five-number-summary l)
  (receive (min max) (apply min&max l)
    (values min
            (quantile 1/4 l)
            (quantile 1/2 l)
            (quantile 3/4 l)
            max)))

(define (sqr x) (* x x))

;; todo: name?
(define (stddev l)
  (let ((sum  (apply + l))
        (sum² (apply + (map sqr l)))
        (cnt  (length l)))
    (sqrt (- (/ sum² cnt) (sqr (/ sum cnt))))))

;; todo: name?
(define (stddev-bessel-corrected l)
  (let ((sum  (apply + l))
        (sum² (apply + (map sqr l)))
        (cnt  (length l)))
    (sqrt (- (/ sum² (- cnt 1)) (/ (sqr sum) (* (- cnt 1) cnt))))))
