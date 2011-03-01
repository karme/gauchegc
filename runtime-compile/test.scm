#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(use gauche.time)
(test-start "runtime-compile")
(use runtime-compile)
(test-module 'runtime-compile)

(define body '(sqrt (+ (* (- x1 x2) (- x1 x2))
                       (* (- y1 y2) (- y1 y2)))))

(define norm-2
  (eval `(lambda(x1 y1 x2 y2) ,body)
        (interaction-environment)))

(define (timed thunk)
  (let1 t (make <real-time-counter>)
    (with-time-counter t
      (thunk))
    (time-counter-value t)))

(test* "compile-and-load"
       #t
       (begin
         (compile-and-load
          `((define (norm-2p x1 y1 x2 y2)
              ,body)
            (inline-stub
             (declcode
              (.include |<math.h>|))
             (define-cproc norm-2c
               (x1::<double> y1::<double> x2::<double> y2::<double>)
               ::<number> :fast-flonum ;; :constant
               (result (Scm_MakeFlonum ,body)))))
          '(norm-2p norm-2c))
         ;; #?=(disasm norm-2p)
         ;; #?=(disasm norm-2)
         (and
          (equal? (norm-2c 1. 1. 2. 2.)
                  (norm-2  1. 1. 2. 2.))
          (equal? (norm-2c 1. 1. 2. 2.)
                  (norm-2p 1. 1. 2. 2.)))))

;; todo: fragile
(test* "timed"
       #t
       (< (* 1.5 (timed
                  (lambda()
                    (dotimes (i 1000000)
                      (norm-2c 1. 1. 2. 2.)))))
          (timed
           (lambda()
             (dotimes (i 1000000)
               (norm-2  1. 1. 2. 2.))))))

(test* "cise-compile-and-load"
       #t
       (begin
         (cise-compile-and-load
          `((declcode
             (.include |<math.h>|))
            (define-cproc twice
              (x::<double>)
              ::<number> :fast-flonum ;; :constant
              (result (Scm_MakeFlonum (* 2 x)))))
          '(twice))
         (equal? (twice 2) 4.0)))

(test-end)
