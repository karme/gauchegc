#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
GC_PRINT_STATS=1
#export GC_PRINT_STATS
:|#
:; exec gosh -Fpost-gc-hook-via-runtime-compile -I. -I../runtime-compile -- $0 "$@"
;; :; exec gosh -Fpost-gc-hook-via-runtime-compile -fno-lambda-lifting-pass -I. -I../runtime-compile -- $0 "$@"
(use gauche.test)
(use post-gc-hook)
(use gauche.vm.insn)
(use util.match)
(use gauche.vport)

(test-start "post-gc-hook")

(define *hook-called* 0)

(define (run-gc-and-post-gc-hooks)
  ;;todo: grgrg
  (cond-expand
   (post-gc-hook-via-vport
    (dotimes (i 2)
      (make <virtual-output-port>))
    #?=(gc))
   (else
    #t))
  (gc)
  (run-post-gc-hooks))

(let1 r ((with-post-gc-hook (lambda()
                              (print "lambda called")
                              (+ 1 2))
                            (lambda()
                              (inc! *hook-called*)
                              (print "hook called")
                              )))
  (run-gc-and-post-gc-hooks)
  (test* "hook called? (1)"
         1
         (and (= r 3) *hook-called*)))

(define (foo)
  ((with-post-gc-hook (lambda()
                        (print "lambda2 called")
                        (+ 1 2))
                      (lambda()
                        (print "hook2 called")
                        (inc! *hook-called*)))))

(let1 r (foo)
  (run-gc-and-post-gc-hooks)  
  (test* "hook called? (2)"
         2
         (and (= r 3) *hook-called*)))
;;(disasm foo)

(cond-expand
 (post-gc-hook-via-vport
  #t)
 (post-gc-hook-via-runtime-compile  
  #t)
 (post-gc-hook-via-c-wrapper
  #t)
 (else
  (define (vector-test-2)
    (let1 v (with-post-gc-hook (make-vector 10)
                               (lambda()
                                 (print "vector hook called")
                                 (inc! *hook-called*)))
      ;; (set! (ref v 4) 3)
      ;; (+ (ref v 4) 0)
      3))
  
  (define (vector-test)
    (let1 r (vector-test-2)
      (run-gc-and-post-gc-hooks)
      (test* "hook called? (3)"
             3
             (and (= r 3) *hook-called*))))
  
  (vector-test)
  (disasm vector-test)))

(test-end)
