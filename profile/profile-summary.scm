#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.sequence)
(use util.queue)
(use gauche.process)
(use srfi-19)

(define (avg . l)
  (/ (apply + l) (size-of l)))

(define (queue-unique l)
  (let1 r (make-queue)
    (for-each (cut enqueue-unique! r equal? <>) l)
    (queue->list r)))

(define unique queue-unique)

(define clear-terminal
  (let1 s (process-output->string `(tput clear))
    (lambda()
      (display s)
      (flush))))

(define (print-totals all)
  (for-each (lambda(x)
              (print (if-let1 s (car (assoc-ref x 'source #f))
                       (string-append (car s) ":" (x->string (cadr s)) ":  ")
                       "unknown: ")
                     (filter (lambda(x) (not (equal? (car x) 'source)))
                             x)))
            (sort (filter boolean (map (lambda(l)
                                         (let1 times (filter boolean
                                                             (map (lambda(x) (car (assoc-ref x 'runtime '(#f)))) l))
                                           (if (null? times)
                                             #f
                                             `((source ,(car (assoc-ref (car l) 'source)))
                                               (count ,(size-of l))
                                               (total-time ,(apply + times))
                                               (min-time ,(apply min times))
                                               (max-time ,(apply max times))
                                               (avg-time ,(apply avg times))))))
                                       (group-collection all
                                                         :key (lambda(x) (assoc-ref x 'source))
                                                         :test equal?)))
                  (lambda(x y) (> (car (assoc-ref x 'total-time))
                                  (car (assoc-ref y 'total-time)))))))

(define (callgraph-edges all)
  (group-collection (map (lambda(x)
                           `((source ,(if (not (null? (car (assoc-ref x 'stack))))
                                        (caar (assoc-ref x 'stack))
                                        (list (list "start" 0) "start")))
                             (target ,(list (car (assoc-ref x 'source))
                                            (car (assoc-ref x 'form))))
                             (runtime ,(car (assoc-ref x 'runtime)))))
                         (filter (lambda(x)
                                   (car (assoc-ref x 'runtime '(#f))))
                                 all))
                    :key (lambda(e)
                           (list (car (assoc-ref e 'source))
                                 (car (assoc-ref e 'target))))
                    :test equal?))

(define (print-graph all)
  (let ((node-name (lambda(n)
                     (if (car n)
                       (string-append (caar n) ":" (x->string (cadar n)))
                       "unknown" ;; todo
                       ))))
    (with-output-to-process
     '(graph-easy --as boxart)
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
                 (callgraph-edges all)))
     :output (current-output-port))))

(define (main args)
  (let ((all '())
        (start-time (current-time))
        (update-interval 1))
    (while (read) (compose not eof-object?) => l
           (push! all l)
           (let1 end-time (current-time)
             (when (> (time->seconds (time-difference end-time start-time)) update-interval)
               (set! start-time end-time)
               (clear-terminal)
               ;; todo: might be slower than update-interval ;-)
               (print-totals all)
               (print-graph all)))))
  0)
