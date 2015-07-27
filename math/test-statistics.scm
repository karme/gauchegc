#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I.. -- $0 "$@"

(use gauche.test)
(use srfi-1)

(test-start "math.statistics")

(use math.statistics)
(test-module 'math.statistics)

(test* "five-number-summary for 0 to 2"
       '(0 0 1 2 2)
       (receive l (five-number-summary (iota 3)) l))

(test* "five-number-summary for 2 to 0"
       '(0 0 1 2 2)
       (receive l (five-number-summary (reverse (iota 3))) l))

(test* "five-number-summary for 0 to 3"
       '(0 1/2 3/2 5/2 3)
       (receive l (five-number-summary (iota 4)) l))

(test* "five-number-summary for 3 to 0"
       '(0 1/2 3/2 5/2 3)
       (receive l (five-number-summary (reverse (iota 4))) l))

(test* "five-number-summary for 0 to 100"
       '(0 25 50 75 100)
       (receive l (five-number-summary (iota 101)) l))

(test* "five-number-summary for 100 to 0"
       '(0 25 50 75 100)
       (receive l (five-number-summary (reverse (iota 101))) l))

(define (n-times n l)
  (append-map (lambda _ l) (iota n)))

(test* "five-number-summary for 100* 0 to 100"
       '(0 25 50 75 100)
       (receive l (five-number-summary (n-times 100 (iota 101))) l))

(test* "five-number-summary for 100* 100 to 0"
       '(0 25 50 75 100)
       (receive l (five-number-summary (n-times 100 (reverse (iota 101)))) l))

(test* "five-number-summary for 0 1 1 1 2"
       '(0 1 1 1 2)
       (receive l (five-number-summary '(0 1 1 1 2)) l))

(test* "68/100 quantile"
       68
       (quantile 68/100 (iota 101)))

(test* "stddev for 0 to 100"
       29
       (round->exact (stddev (iota 101))))

(test* "stddev-bessel-corrected for 0 to 100"
       29
       (round->exact (stddev-bessel-corrected (iota 101))))

(test-end)
