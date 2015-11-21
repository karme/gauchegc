#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use sxml.adaptor) ;; for assert
(use gauche.generator)
(use gauche.process)
(use math.const)
(use runtime-compile)

(compile-and-load
 '((inline-stub
    (declcode
     (.include "math.h"))

    (define-cproc c-cosl (x::<double>) ::<double>
      (let* ((ld::(long double) (cast (long double) x))
             (ld2::(long double) (cosl ld))
             (r::double (cast double ld2)))
        ;;(printf "from C: %.40Lf -> %.40Lf\n" ld ld2)
        (result r)))))
 '(c-cosl) :cflags "-v -Wall -Wextra -lm")

(compile-and-load
 '((inline-stub
    (declcode
     (.include "mpfr.h"))

    (define-cproc mpfr-cos->double (d::<double> prec::<int>) ::<double>
      (let* ((x::mpfr_t))
        (mpfr_init2 x prec)
        (mpfr_set_d x d MPFR_RNDN)
        (mpfr_cos x x MPFR_RNDN)
        (result (mpfr_get_d x MPFR_RNDN))))
    (define-cproc mpfr-cos->string (d::<double> prec::<int>) ::<string>
      (let* ((x::mpfr_t))
        (mpfr_init2 x prec)
        (mpfr_set_d x d MPFR_RNDN)
        (mpfr_cos x x MPFR_RNDN)
        (let* ((b::char*)
               (i::int (mpfr_asprintf (& b) "%Re" x))
               (r (SCM_MAKE_STR_COPYING b)))
          (mpfr_free_str b)
          (result r))))))
 '(mpfr-cos->double mpfr-cos->string)
 :libs "-lmpfr")

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

(define (approximate-exp x prec)
  (∑ (gtake-while (^k (> k prec)) (exp-sequence-gen x))))

(define (sin-sequence-gen x)
  (gunfold (^s #f)
           (^s (car s))
           (^s (cons (/ (* -1 (car s) x x)
                        (* (- (cdr s) 1) (cdr s)))
                     (+ (cdr s) 2)))
           (cons x 3)))

(define (approximate-sin x prec)
  (∑ (gtake-while (^k (> (abs k) prec)) (sin-sequence-gen x))))

(define (cos-sequence-gen x)
  (gunfold (^s #f)
           (^s (car s))
           (^s (cons (/ (* -1 (car s) x x)
                        (* (- (cdr s) 1) (cdr s)))
                     (+ (cdr s) 2)))
           (cons 1 2)))

(define (approximate-cos-small x prec)
  (assert (< (abs x) 4))
  (∑ (gtake-while (^k (> (abs k) prec)) (cos-sequence-gen x))))

(define (atan-sequence-gen x)
  (gunfold (^s #f)
           (^s (/ (car s)
                  (cdr s)))
           (^s (cons (* -1 (car s) x x)
                     (+ (cdr s) 2)))
           (cons x 1)))

(define (approximate-atan x prec)
  (∑ (gtake-while (^k (> (abs k) prec)) (atan-sequence-gen x))))

(define (approximate-pi-slow prec)
  (* 4 (approximate-atan 1 (/ prec 4))))

;; John Machin's formula
(define (approximate-pi prec)
  (* 4 (- (* 4 (approximate-atan 1/5 (/ prec (* 4 4 2)))) (approximate-atan 1/239 (/ prec (* 4 4 2))))))

(define (approximate-cos x prec)
  ;; argument reduction
  ;; todo: hack / not good
  (receive (n m) (div0-and-mod0 x (* 2 (exact pi)))
    (let1 pi (approximate-pi (/ prec (max n 1)))
      (receive (n m) (div0-and-mod0 x (* 2 pi))
        (let1 pi (approximate-pi (/ prec (max n 1) 2))
          (assert (= n (div0 x (* 2 pi))))
          (approximate-cos-small (mod0 x (* pi 2)) prec))))))

;; todo: note: truncating!
(define (display-rational-digits e prec . args)
  (let-optionals* args ((base 10))
    (define (display-rational-digits-2 e prec base i)
      (receive (q r) (quotient&remainder (numerator e) (denominator e))
        (display (number->string q base))
        (when (zero? i) (display "."))
        (when (> prec 0)
          (display-rational-digits-2 (* (/ r (denominator e)) base) (- prec 1) base (+ i 1)))))

    (assert (exact? e))
    (when (< e 0)
      (display "-"))
    (display-rational-digits-2 (abs e) prec base 0)))

(define (print-rational-digits . l)
  (apply display-rational-digits l)
  (newline))

(define (rational-digits->string . l)
  (with-output-to-string (cute apply display-rational-digits l)))

(define (approximation->string f scale)
  (lambda l
    (let ((s1 (rational-digits->string (apply f (append l (list (/ 1 (expt 10 scale))))) scale))
          ;;(s2 (rational-digits->string (apply f (append l (list (/ 1 (expt 10 (+ scale 1)))))) scale))
          )
      ;;(assert (equal? s1 s2))
      s1)))

(define (bc x scale)
  (call-with-process-io '(bc -s -l)
                        (lambda(in out)
                          (with-ports in out #f
                                      (lambda()
                                        (print #`"scale=,scale")
                                        (print x)
                                        (let1 r (read-line)
                                          (print "quit")
                                          r))))))

;; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
(define (hero x a)
  (/ (+ x (/ a x)) 2))

(define (approximate-sqrt a prec)
  (let1 r (let loop ((x (/ a 2)))
            (let1 x₊₁ (hero x a)
              (if (and (> x x₊₁) (< (- x x₊₁) prec)) ;; todo: is that correct?!
                x₊₁
                (loop x₊₁))))
    (assert (< (abs (- a (* r r))) prec))
    r))

;; http://www.craig-wood.com/nick/articles/pi-chudnovsky/
(define (chudnovsky-series-gen)
  (gunfold (^s (zero? (car s)))
           (^s (cons (car s) (* (car s) (cdr s))))
           (^s (let1 k (cdr s)
                 (cons (/ (* (car s) -1 (- (* 6 k) 5) (- (* 2 k) 1) (- (* 6 k) 1))
                          (* k k k (/ (* 640320 640320 640320) 24)))
                       (+ k 1))))
           (cons 1 1)))

;; todo: buggy?! precision?! crap
;; (define (approximate-pi-chudnovsky prec)
;;   (let1 n (* 426880 (approximate-sqrt 10005 (/ prec 426880 10000)))
;;     #?=(inexact n)
;;     #?=(inexact (/ prec 2 13591409 n))
;;     #?=(inexact (/ prec 2 545140134 n))
;;     (let1 sums (generator-fold (lambda(n o)
;;                                  #?=n
;;                                  (cons (+ (car n) (car o))
;;                                        (+ (cdr n) (cdr o))))
;;                                (cons 1 0)
;;                                (gdrop (gtake-while (lambda(x)
;;                                                      ;; todo: crap
;;                                                      (or #?=(> (abs (car x)) (/ prec 2 13591409 n 1000))
;;                                                          #?=(> (abs (cdr x)) (/ prec 2 545140134 n 1000))))
;;                                                    (chudnovsky-series-gen)) 1))
;;       (/ n
;;          (+ (* 13591409 (car sums)) (* 545140134 (cdr sums)))))))

;; http://en.wikipedia.org/wiki/Gauss-Legendre_algorithm
;; todo: hmm how do preca and precb relate?!
;; slow / buggy?
;; (define (approximate-pi-gauss-legendre preca precb)
;;   (define (f a b t p)
;;     (if (< (abs (- a b)) precb)
;;       (/ (* (+ a b) (+ a b))
;;          (* 4 t))
;;       (let ((a+1 (/ (+ a b) 2))
;;             (b+1 (approximate-sqrt (* a b) preca)))
;;         (let ((t+1 (- t (* p (* (- a a+1) (- a a+1)))))
;;               (p+1 (* 2 p)))
;;           (f a+1 b+1 t+1 p+1)))))
;;   (let1 r (f 1 (/ 1 (approximate-sqrt 2 preca)) 1/4 1)
;;     (assert (< (abs (- (inexact r) pi)) precb))
;;     r))
;;#?=(inexact (approximate-pi-gauss-legendre 1/10000 1/10000))
;; 3.1415926|462157087
;; (inexact (approximate-pi-gauss-legendre 1/1000000 1/100000))
;; slow as hell?!
;; 3.1415926535|95287

(define (main args)
  (let* ((scale 30))
    #?=((approximation->string (lambda _ 2/3) scale))
    #?=(bc "2/3" scale)
    #?=(inexact 2/3)

    #?=((approximation->string approximate-exp scale) 1)
    #?=(bc "e(1)" scale)
    #?=(exp 1)
    
    #?=((approximation->string approximate-sin scale) 3)
    #?=(bc "s(3)" scale)
    #?=(sin 3)

    #?=((approximation->string approximate-cos-small scale) 3)
    #?=(bc "c(3)" scale)
    #?=(cos 3)

    #?=((approximation->string approximate-cos-small scale) (+ 3 1/10))
    #?=(bc "c(3.1)" scale)
    #?=(cos 3.1)

    (let1 test-cos (lambda(x)
                     #?=x
                     ;; errors quite large
                     #?=(cos x)
                     ;; gauche float printer is nice => no need for this
                     ;;#?=(rational-digits->string (exact (cos x)) scale)
                     ;; broken for 10²²?!
                     #?=(c-cosl x)
                     #?=(mpfr-cos->double x 128)
                     #?=(mpfr-cos->string x 128)
                     ;; bad idea (precision of 2pi too bad)
                     ;;#?=(cos (mod0 x 2pi))
                     
                     #?=((approximation->string approximate-cos scale) x)
                     ;; note: bc doesn't use higer precision pi in argument reduction?
                     #?=(bc #`"c(,|x|)" scale))
      (test-cos 3)
      (test-cos 200)
      (test-cos (expt 10 22))))
  0)
