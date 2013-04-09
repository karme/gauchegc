#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use sxml.adaptor) ;; for assert
(use gauche.generator)
(use gauche.process)

(define (power x n)
  (define (power-2 x n)
    (if (zero? n)
      1
      (* x (power-2 x (- n 1)))))

  (assert (and (exact? n)) (>= n 0))
  (power-2 x n))

(define (factorial n)
  (define (factorial-2 n)
    (if (zero? n)
      1
      (* n (factorial-2 (- n 1)))))
  
  (assert (and (exact? n)) (>= n 0))
  (factorial-2 n))

(define ! factorial)

(define (power-sequence-gen x)
  (gunfold (^s #f) (^s s) (^s (* s x)) 1))

(define (factorial-sequence-gen)
  (gunfold (^s #f)
           (^s (car s))
           (^s (cons (* (car s) (cdr s))
                     (+ (cdr s) 1)))
           (cons 1 1)))

(define (exp-sequence-gen x)
  (gmap / (power-sequence-gen x) (factorial-sequence-gen)))

(define ∑ (cute generator-fold + 0 <>))

;; todo: more sensible precision logic?!
(define (approximate-exp x prec)
  (∑ (gtake (exp-sequence-gen x) prec)))

(define (sin-sequence-gen x)
  (gunfold (^s #f)
           (^s (car s))
           (^s (cons (/ (* -1 (car s) x x)
                        (* (- (cdr s) 1) (cdr s)))
                     (+ (cdr s) 2)))
           (cons x 3)))

(define (approximate-sin x prec)
  (∑ (gtake (sin-sequence-gen x) prec)))

(define (cos-sequence-gen x)
  (gunfold (^s #f)
           (^s (car s))
           (^s (cons (/ (* -1 (car s) x x)
                        (* (- (cdr s) 1) (cdr s)))
                     (+ (cdr s) 2)))
           (cons 1 2)))

(define (approximate-cos x prec)
  (∑ (gtake (cos-sequence-gen x) prec)))

;; todo: note: truncating!
(define (print-rational e . args)
  (let-optionals* args ((prec 30)
                        (base 10))
    (define (print-rational-2 e prec base i)
      (receive (q r) (quotient&remainder (numerator e) (denominator e))
        (display (number->string q base))
        (when (zero? i) (display "."))
        (when (> prec 0)
          (print-rational-2 (* (/ r (denominator e)) base) (- prec 1) base (+ i 1)))))

    (assert (exact? e))
    (when (< e 0)
      (display "-"))
    (print-rational-2 (abs e) prec base 0)
    (newline)))

(define (bc x . args)
  (let-optionals* args ((scale 30))
    (call-with-process-io '(bc -l)
                          (lambda(in out)
                            (with-ports in out #f
                                        (lambda()
                                          (print #`"scale=,scale")
                                          (print x)
                                          (let1 r (read-line)
                                            (print "quit")
                                            r)))))))

(define (main args)
  (let1 prec 31
    (print-rational 2/3)
    (print (bc "2/3"))
    (print (inexact 2/3))
    
    (print-rational (approximate-exp 1 prec))
    (print (bc "e(1)"))
    (print (exp 1))
    
    (print-rational (approximate-sin 3 prec))
    (print (bc "s(3)"))
    (print (sin 3))
    
    (print-rational (approximate-cos 3 prec))
    (print (bc "c(3)"))
    (print (cos 3))
    
    (print-rational (approximate-cos (+ 3 1/10) prec))
    (print (bc "c(3.1)"))
    (print (cos 3.1)))
0)
