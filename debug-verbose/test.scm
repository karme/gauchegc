#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
DEBUG=""
[ "x$1" = "x-v" ] && DEBUG="-udebug-verbose"
if [ "x$1" = "x-vv" ]; then
DEBUG="-udebug-verbose"
DEBUG_VERBOSE_GLOBAL=1
export DEBUG_VERBOSE_GLOBAL
fi        
# |#
:; exec gosh -I. $DEBUG -- $0 "$@"

;; uh :-(
(when (sys-getenv "DEBUG_VERBOSE_GLOBAL")
  (debug-verbose-global-hack))

(use foo)
(use bar)

(define (main args)
  #?=(+ 2 3)
  (foo-proc)
  (bar-proc)
  0)
