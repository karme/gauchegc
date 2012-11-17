#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.sequence)

(define (main args)
  (let1 all '()
    (while (read) (compose not eof-object?) => l
           (push! all l))
    (let* ((source-times (map
                          (lambda(l)
                            (list
                             (car (assoc-ref (car l) 'source))
                             (apply + (map (lambda(x) (car (assoc-ref x 'runtime '(0)))) l))))
                          (group-collection all
                                            :key (lambda(x) (assoc-ref x 'source))
                                            :test equal?)))
           ;;(total-time (apply + (map cadr source-times))) ;; todo: wrong
           )
      (for-each (lambda(x)
                  (print (caar x) ":" (cadar x) ":  " (cadr x)
                         ;;" " (* (/ (cadr x) total-time) 100)
                         ))
                (sort source-times
                      (lambda(x y) (> (cadr x) (cadr y)))))))
  0)
