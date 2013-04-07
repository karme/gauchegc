#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use sxml.adaptor) ;; for assert
(use gauche.generator)

(define (power x n)
  (assert (and (exact? n)) (>= n 0))
  (if (zero? n)
    1
    (* x (power x (- n 1)))))

(define (factorial n)
  (assert (and (exact? n)) (>= n 0))
  (if (zero? n)
    1
    (* n (factorial (- n 1)))))

(define ! factorial)

(define (power-series-v1 x)
  (define (gen)
    (let1 c 1
      (lambda()
        (let1 r c
          (set! c (* c x))
          r))))
  (generator->lseq (gen)))

(define (power-series-gen x)
  (gunfold (^s #f) (^s s) (^s (* s x)) 1))

(define (factorial-series-gen)
  (gunfold (^s #f)
           (^s (car s))
           (^s (cons (* (car s) (cdr s))
                     (+ (cdr s) 1)))
           (cons 1 1)))

(define (exp-series-gen x)
  (gmap / (power-series-gen x) (factorial-series-gen)))

(define (approximate-exp-v1 x prec)
  ((rec (f x n maxn)
        (if (>= n maxn)
          0
          (+ (/ (power x n) (! n)) (f x (+ n 1) maxn))))
   x 0 prec))

(define ∑ (cute generator-fold + 0 <>))

;; todo: more sensible precision logic?!
(define (approximate-exp x prec)
  (∑ (gtake (exp-series-gen x) prec)))

;; todo: generator versions

(define (approximate-sin x prec)
  ((rec (f x n maxn)
        (if (>= n maxn)
          0
          (+ (* (if (even? n) 1 -1)
                (/ (power x (+ (* 2 n) 1))
                   (! (+ (* 2 n) 1))))
             (f x (+ n 1) maxn))))
   x 0 prec))

(define (approximate-cos x prec)
  ((rec (f x n maxn)
        (if (>= n maxn)
          0
          (+ (* (if (even? n) 1 -1)
                (/ (power x (* 2 n))
                   (! (* 2 n))))
             (f x (+ n 1) maxn))))
   x 0 prec))

(define (main args)
  #?=(approximate-exp 1 100)
  #?=(inexact (approximate-exp 1 100))
  #?=(exp 1)
  #?=(approximate-sin 3 100)
  #?=(inexact (approximate-sin 3 100))
  #?=(sin 3)
  #?=(approximate-cos 3 100)
  #?=(inexact (approximate-cos 3 100))
  #?=(cos 3)
  0)
