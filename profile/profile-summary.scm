#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.sequence)

(define (avg . l)
  (/ (apply + l) (size-of l)))

(define (main args)
  (let1 all '()
    (while (read) (compose not eof-object?) => l
           (push! all l))
    (let* ((grouped (group-collection all
                                            :key (lambda(x) (assoc-ref x 'source))
                                            :test equal?))
           (source-times (map (lambda(l)
                                (let1 times (filter boolean
                                                    (map (lambda(x) (car (assoc-ref x 'runtime '(#f)))) l))
                                  `((source ,(car (assoc-ref (car l) 'source)))
                                    (count ,(size-of l))
                                    (total-time ,(apply + times))
                                    (min-time ,(apply min times))
                                    (max-time ,(apply max times))
                                    (avg-time ,(apply avg times)))))
                              grouped))
           ;;(total-time (apply + (map cadr source-times))) ;; todo: wrong
           )
      (for-each (lambda(x)
                  (print (if-let1 s (car (assoc-ref x 'source #f))
                           (string-append (car s) ":" (x->string (cadr s)) ":  ")
                           "unknown: ")
                         (filter (lambda(x) (not (equal? (car x) 'source)))
                                 x)
                         ;;(car (assoc-ref x 'total-time))
                         ;;" " (* (/ (cadr x) total-time) 100)
                         ))
                (sort source-times
                      (lambda(x y) (> (car (assoc-ref x 'total-time))
                                      (car (assoc-ref y 'total-time))))))))
  0)
