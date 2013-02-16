#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I../runtime-compile -I. -- $0 "$@"
(use sxml2xml)

(define (main args)
  (let1 writer (make-xml-writer (current-output-port))
    (until (read) eof-object? => expr
           (xml-writer-write writer expr))
    (close-xml-writer writer))
  0)
