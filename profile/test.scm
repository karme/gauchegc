#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
DEBUG=""
[ "x$1" = "x-v" ] && DEBUG="-uprofile"
if [ "x$1" = "x-vv" ]; then
DEBUG="-uprofile"
DEBUG_VERBOSE_GLOBAL=1
export DEBUG_VERBOSE_GLOBAL
fi        
# |#
:; exec gosh -I. -I../runtime-compile -I../gc-hack $DEBUG -- $0 "$@"

;; uh :-(
(when (sys-getenv "DEBUG_VERBOSE_GLOBAL")
  (profile-global-hack)
  (set! (port-buffering (current-error-port)) :full))

(use foo)
(use bar)

(define (factorial n)
  (sys-sleep 1)
  (if (= n 1)
    1
    #?=(* n (factorial (- n 1)))))

(define (f3)
  #?=(factorial 3))

(define (rungc)
  (gc))
  
(define (test)
  #?=(+ 2 3)
  #?=(foo-proc)
  #?=(bar-proc "k" "l")
  #?=(factorial 4)
  #?=(f3)
  #?=(rungc)
  (guard (e [else #f])
         #?=(bar-error-proc))
  )

(define (main args)
  #?=(test)
  0)
