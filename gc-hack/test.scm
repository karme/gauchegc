#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../runtime-compile -- $0 "$@"

(use gauche.uvector)
(use gauche.test)
(use gc-hack)

(test-start "gc-hack")

(test* "gc-set-warn-proc" #t
       (let1 called #f
         (gc-set-warn-proc
          (lambda(msg arg)
            (with-output-to-port (current-error-port)
              (lambda()
                (set! called #t)
                (print "SCHEME GC WARNING HANDLER HACK:\n" msg arg)
                (%vm-show-stack-trace (vm-get-stack-trace-lite))))))
         (dotimes (i 10)
           (make-u8vector 200000000)
           (gc)
           #?=(gc-get-gc-no)
           #?=(gc-stat))
         called))

(test* "gc-get-gc-no"
       1
       (let1 s (gc-get-gc-no)
         (gc)
         (- (gc-get-gc-no) s)))

(test-end)
