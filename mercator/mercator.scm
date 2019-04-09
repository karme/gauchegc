;;;
;;; mercator projections
;;; written to thank Peter Osborne for his fine article
;;; "THE MERCATOR PROJECTIONS"
;;; http://www.mercator99.webspace.virginmedia.com/
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
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

(define-module mercator
  (use sxml.adaptor) ;; for assert
  (use math.const)
  (export earth-sphere-radius
          deg->rad
          rad->deg
          spherical->cartesian
          cartesian->spherical
          great-circle-distance
          spherical-mercator
          spherical-mercator⁻¹
          ))
(select-module mercator)

;; mapping notation used by Peter Osborne to unicode:
;; http://milde.users.sourceforge.net/LUCR/Math/unimathsymbols.xhtml
;; (don't take that too serious, in fact I dislike the habit of
;; mathematicians to obfuscate their "code". But at least it should be
;; easy to automatically replace the symbols with words, if you wish.)
;;
;; │ usage              │ name           │ latex     │ unicode │ alternatives │
;; │ mercator parameter │ PSI            │ \psi      │ ψ       │ Ψ            │
;; │ longitude          │ LAMBDA         │ \lambda   │ λ       │              │
;; │ latitude           │ PHI            │ \phi      │ ϕ       │ Φφ           │
;; │ Pi                 │ PI             │ \pi       │ π       │ Π            │
;; │                    │ SUBSCRIPT ZERO │ _0        │ ₀       │              │
;; │ inverse            │ SUPERSCRIPT -1 │ -1, ^{-1} │ ⁻¹      │              │

(define-constant π pi)
(define-constant ½π pi/2)
(define-constant π/2 pi/2)
(define-constant ¼π pi/4)
(define-constant π/4 pi/4)

;;; Radius of the sphere

(define-constant triaxial-mean-radius   6371008.8)
(define-constant equal-volume-radius    6371000.8)
(define-constant equal-area-radius      6371007.2)
;; good enough
(define-constant earth-sphere-radius    6371000)
(define-constant default-radius         earth-sphere-radius)

(define sin⁻¹ asin)
(define cos⁻¹ acos)
(define tan⁻¹ atan)
(define sinh⁻¹ asinh)
(define cosh⁻¹ acosh)
(define tanh⁻¹ atanh)
(define (arsech x) (log (/ (+ 1 (sqrt (- 1 (* x x)))) x)))
(define (arcsch x) (log (+ (/ 1 x) (sqrt (+ (/ 1 (* x x)) 1)))))
(define (sech x) (/ 1 (cosh x)))
(define sech⁻¹ arsech)
(define (csch x) (/ 1 (sinh x)))
(define csch⁻¹ arcsch)
(define (sec x) (/ 1 (cos x)))
(define (arcsec x) (acos (/ 1 x)))
(define sec⁻¹ arcsec)
(define (gd x) (* 2 (atan (tanh (/ x 2)))))
(define (gd⁻¹ x) (* 1/2 (log (/ (+ 1 (sin x)) (- 1 (sin x))))))
(define (sqr x) (* x x))
(define ² sqr)
(define vec- (map$ -))
(define dot-product (.$ (apply$ +) (map$ *)))
(define (norm x) (sqrt (dot-product x x)))
(define (deg->rad x) (* (/ x 180) π))
(define (rad->deg x) (* (/ x π) 180))


;; spherical to cartesian coordinates
(define (spherical->cartesian λ ϕ . args)
  (let-optionals* args ((h 0)
                        (R default-radius))
    (define (p ϕ) (* (+ R h) (cos ϕ)))
    (list (* (p ϕ) (cos λ))
          (* (p ϕ) (sin λ))
          (* (+ R h) (sin ϕ)))))

;; cartesian to spherical coordinates
(define (cartesian->spherical x y z . args)
  (let-optionals* args ((R default-radius))
    (list (atan y x)
          (atan (/ z (norm (list x y))))
          (- (norm (list x y z)) R))))

;; todo: good enough?
(define (great-circle-distance p q . args)
  (let-optionals* args ((R default-radius))
    (assert (and (list? p) (= (length p) 2)))
    (assert (and (list? q) (= (length q) 2)))
    (* R (cos⁻¹ (dot-product (spherical->cartesian (car p) (cadr p) 0 1)
                             (spherical->cartesian (car q) (cadr q) 0 1))))))

(define (mercator-parameter ϕ)
  ;; choose one of:
  ;; (log (tan (+ (/ ϕ 2) π/4)))
  ;; (* 1/2 (log (/ (+ 1 (sin ϕ)) (- 1 (sin ϕ)))))
  ;; (log (+ (sec ϕ) (tan ϕ)))
  ;; (sinh⁻¹ (tan ϕ))
  ;; (sech⁻¹ (cos ϕ))
  ;; (cosh⁻¹ (sec ϕ))
  ;; (tanh⁻¹ (sin ϕ))
  ;; (gd⁻¹ ϕ)
  (gd⁻¹ ϕ)
  )

(define ψ mercator-parameter)

(define (inverse-parameter ψ)
  ;; choose one of:
  ;; (- (* 2 (tan⁻¹ (exp ψ))) π/2)
  ;; (sin⁻¹ (tanh ψ))
  ;; (tan⁻¹ (sinh ψ))
  ;; (cos⁻¹ (sech ψ))
  ;; (sec⁻¹ (cosh ψ))
  ;; (gd ψ)
  (gd ψ)
  )

(define ψ⁻¹ inverse-parameter)

(define (normal-mercator-sphere λ ϕ . args)
  (let-optionals* args ((R default-radius)
                        (k₀ 1)
                        (λ₀ 0))
    (list (* k₀ R (- λ λ₀))
          (* k₀ R (ψ ϕ)))))
(define nms normal-mercator-sphere)
(define spherical-mercator normal-mercator-sphere)

(define (normal-mercator-sphere⁻¹ x y . args)
  (let-optionals* args ((R default-radius)
                        (k₀ 1)
                        (λ₀ 0))
    (list (+ λ₀ (/ x (* k₀ R)))
          (ψ⁻¹ (/ y k₀ R)))))
(define nms⁻¹ normal-mercator-sphere⁻¹)
(define spherical-mercator⁻¹ normal-mercator-sphere⁻¹)

(define (parallel-distance λ₁ λ₂ ϕ . args)
  (let-optionals* args ((R default-radius))
    (* R (cos ϕ) (- λ₂ λ₁))))

(define (loxodrome-distance α ϕ₁ ϕ₂ . args)
  (let-optionals* args ((R default-radius))
    (assert (not (= ϕ₂ ϕ₁)))
    (* R (sec α) (- ϕ₂ ϕ₁))))

;; mercator sailing p.39
(define (inv p q)
  (let1 α (atan (- (car q) (car p))
                (- (ψ (cadr q)) (ψ (cadr p))))
    (values α
            (if (= (cadr p) (cadr q))
              (parallel-distance (car p) (car q) (cadr p))
              (loxodrome-distance α (cadr p) (cadr q))))))

(define (inv° p q)
  (receive (α d) (apply inv (map (^p (map deg->rad p)) (list p q)))
    (values (rad->deg α)
            d)))

;; #?=(inv° '(0 0) '(0 1))
;; #?=(inv° '(0 0) '(1 0))
;; ;; todo
;; #?=(inv° '(0 0) '(1 0.00000000000000000000001))
;; #?=(inv° '(0 0) '(0 -1))
;; #?=(inv° '(0 0) '(-1 0))

;; #?=(/ (* (apply inv (map (^p (map deg->rad p)) '((0 0) (0 -10)))) 180.) pi)
;; #?=(/ (* (apply inv (map (^p (map deg->rad p)) '((0 0) (10 0)))) 180.) pi)
;; #?=(/ (* (apply inv (map (^p (map deg->rad p)) '((0 0) (-10 0)))) 180.) pi)
;; #?=(/ (* (apply inv (map (^p (map deg->rad p)) '((0 0) (4 4)))) 180.) pi)
;; #?=(/ (* (apply inv (map (^p (map deg->rad p)) '((10 10) (11 11)))) 180.) pi)
