#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../lru-cache -- $0 "$@"
(use gauche.test)
(test-start "huge-sparse-bitmap")
(use huge-sparse-bitmap)
(test-module 'huge-sparse-bitmap)

(define (test-sequence l r)
  (let* ((bm (make-huge-sparse-bitmap "bitmap.dbm"))
         (get-bit (cute huge-sparse-bitmap-get-bit bm <>))
         (set-bit! (cute huge-sparse-bitmap-set-bit! bm <> #t))
         (unset-bit! (cute huge-sparse-bitmap-set-bit! bm <> #f))
         (do-op (lambda(x) ((case (car x)
                              [(g) get-bit]
                              [(s) set-bit!]
                              [(u) unset-bit!])
                            (cadr x)))))
    (for-each do-op l)
    (let1 r (map do-op r)
      (huge-sparse-bitmap-sync bm) ;; don't forget to sync! (todo: at least do that on gc?!)
      r)))

(sys-unlink "bitmap.dbm")
(test* "bitmap read simple"
       '(#f)
       (test-sequence '((g 1))
                      '((g 1))))
(sys-unlink "bitmap.dbm")
(test* "bitmap read/write simple"
       '(#f #t #t)
       (test-sequence '()
                      '((g 1) (s 1) (g 1))))

(sys-unlink "bitmap.dbm")
(test* "bitmap read/write simple"
       '(#f #t #t #f #f)
       (test-sequence '()
                      '((g 1) (s 1) (g 1) (u 1) (g 1))))

(sys-unlink "bitmap.dbm")
(test* "bitmap persistence"
       '(#t)
       (begin
         (test-sequence '((s 1)) '())
         (test-sequence '() '((g 1)))))

(sys-unlink "bitmap.dbm")
(test* "test huge"
       '(#t #t #t #t)
       (test-sequence `((s ,(ash 1 64)) (s ,(ash 1 128)) (s ,(ash 1 1024)) (s ,(ash 1 (ash 1 16))))
                      `((g ,(ash 1 64)) (g ,(ash 1 128)) (g ,(ash 1 1024)) (g ,(ash 1 (ash 1 16))))))

(test-end)
