#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.sequence)
(use util.queue)

(define (avg . l)
  (/ (apply + l) (size-of l)))

(define (queue-unique l)
  (let1 r (make-queue)
    (for-each (cut enqueue-unique! r equal? <>) l)
    (queue->list r)))

(define unique queue-unique)

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
           (nodes (unique (map (lambda(x)
                                 (list (car (assoc-ref x 'source))
                                       (car (assoc-ref x 'form))))
                               all)))
           (edges (map (lambda(x)
                         `((source ,(if (not (null? (car (assoc-ref x 'stack))))
                                      (caar (assoc-ref x 'stack))
                                      (list (list "start" 0) "start")))
                           (target ,(list (car (assoc-ref x 'source))
                                          (car (assoc-ref x 'form))))
                           (runtime ,(car (assoc-ref x 'runtime)))))
                       (filter (lambda(x)
                                 (car (assoc-ref x 'runtime '(#f))))
                               all)))
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
                                      (car (assoc-ref y 'total-time))))))
      (let ((node-name (lambda(n)
                         (string-append (caar n)
                                        ":"
                                        (x->string (cadar n))))))
        ;; (for-each (lambda(n)
        ;;             (print "[ " (node-name n) " ]"))
        ;;           nodes)
        (with-output-to-file (ref args 1)
          (lambda()
            (print "graph{flow:south}")
            (for-each (lambda(edges)
                        (print "[ " (node-name (car (assoc-ref (car edges) 'source))) " ]"
                               " - "
                               (apply + (map (lambda(e)
                                               (car (assoc-ref e 'runtime)))
                                             edges))
                               " -> "
                               "[ " (node-name (car (assoc-ref (car edges) 'target))) " ]"))
                      (group-collection edges
                                        :key (lambda(e)
                                               (list (car (assoc-ref e 'source))
                                                     (car (assoc-ref e 'target))))
                                        :test equal?)))))))
  0)
