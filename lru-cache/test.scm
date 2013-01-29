#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(test-start "lru-cache")
(use lru-cache)
(test-module 'lru-cache)

(define (test-read-sequence . l)
  (let* ((read-misses 0)
         (c (make-lru-cache (lambda(x)
                              (inc! read-misses)
                              (+ 1 x))
                            (lambda(k v)
                              (error "no valid write-back handler"))
                            :cache-size 4))
         (get (cute (assoc-ref c 'get) <> )))
    (for-each get l)
    (list read-misses ((assoc-ref c 'keys)))))

(test* "lru-cache read simple"
       '(4 (1 2 3 4))
       (test-read-sequence 1 2 3 4))

(test* "lru-cache read old"
       '(4 (2 3 4 1))
       (test-read-sequence 1 2 3 4 1))

(test* "lru-cache read new"
       '(4 (1 2 3 4))
       (test-read-sequence 1 1 2 3 4))

(test* "lru-cache read mixed"
       '(4 (2 1 3 4))
       (test-read-sequence 1 2 3 4 1 1 3 3 4))

(test* "lru-cache read mixed (2)"
       '(4 (3 4 1 2))
       (test-read-sequence 1 2 3 4 1 2))

(test* "lru-cache read mixed (3)"
       '(5 (2 3 4 5))
       (test-read-sequence 1 2 3 4 1 2 3 3 2 1 1 1 2 3 4 4 4 4 4 4 5 5 5 5 5))

(define (test-read-write-sequence l r)
  (let* ((lower-level (make-hash-table 'equal?))
         (read-misses 0)
         (c (make-lru-cache (lambda(x)
                              (inc! read-misses)
                              (or (hash-table-get lower-level x #f)
                                  (+ 1 x)))
                            (lambda(k v)
                              (hash-table-put! lower-level k v))
                            :cache-size 4))
         (get (cute (assoc-ref c 'get) <>))
         (put! (cute (assoc-ref c 'put!) <> <>))
         (do-op (lambda(x) (apply (if (eq? (car x) 'r) get put!) (cdr x)))))
    (for-each do-op l)
    (let1 res (map do-op r)
      (list read-misses
            ((assoc-ref c 'keys))
            res))))

(test* "lru-cache read/write"
       '(4 (1 3 4 2) (0))
       (test-read-write-sequence
        '((r 1) (r 2) (r 3) (r 4) (w 2 0))
        '((r 2))))

(test* "lru-cache read/write (write-back)"
       '(6 (3 4 5 1) (0))
       (test-read-write-sequence
        '((r 1) (w 1 0) (r 2) (r 3) (r 4) (r 5))
        '((r 1))))

(test* "lru-cache write only"
       '(0 (1) (4))
       (test-read-write-sequence
        '((w 1 2) (w 1 3) (w 1 0) (w 1 10) (w 1 20))
        '((w 1 4))))

(test* "lru-cache dirty write because of read test"
       '(5 (3 4 5 1) (2))
       (test-read-write-sequence
        '((w 1 2) (r 2) (r 3) (r 4) (r 5))
        '((r 1))))

(test-end)
