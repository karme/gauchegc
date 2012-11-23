#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; simple profile data analyzer
;;;
;;; todo: cycle detection
;;; s.a.
;;; - gprof:
;;;   info:gprof#Cycles (info "(gprof) Cycles")
;;;   http://sourceware.org/binutils/docs/gprof/Cycles.html
;;; - valgrind/callgrind/kcachegrind:
;;;   http://valgrind.org/docs/manual/cl-manual.html#cl-manual.cycles
;;;
(use gauche.sequence)
(use util.queue)
(use gauche.process)
(use srfi-19)
(use util.list)

(define (avg . l)
  (/ (apply + l) (size-of l)))

(define (queue-unique l)
  (let1 r (make-queue)
    (for-each (cut enqueue-unique! r equal? <>) l)
    (queue->list r)))

(define unique queue-unique)

(define clear-terminal
  (guard [e
          [else
           (lambda _)]]
         (let1 s (process-output->string `(tput clear))
           (lambda()
             (display s)
             (flush)))))

(define (runtime->string x)
  #`",(ceiling->exact (* x 1000))ms")

(define (profile->node-2 x)
  (list (car (assoc-ref x 'source))
        (car (assoc-ref x 'form))))

(define (profile->node x)
  (let1 c (size-of (filter (cute equal? (profile->node-2 x) <>)
                           (car (assoc-ref x 'stack))))
    (if (zero? c)
      (profile->node-2 x)
      (append (profile->node-2 x) (list c)))))

(define (profile->stack-nodes x)
  (if (null? (car (assoc-ref x 'stack)))
    '("start")
    (map
     (lambda(x)
       (if (zero? (cdr x))
         (car x)
         (append (car x) (list (cdr x)))))
     (fold-right (lambda(n o)
                   (acons n (+ (assoc-ref o n -1) 1) o))
                 '()
                 (car (assoc-ref x 'stack))))))

;;(map car (profile->stack-nodes '((time "2012-11-23 11:25:43.638073000+0100") (source #0=(#1="././test.scm" 24)) (pid 17153) (form (#2=(* n (factorial (- n 1))))) (stack ((#0# #2#) (#0# #2#) ((#1# 30) (factorial 6)) ((#1# 36) (test)))) (runtime 3.005024))))

(define (node-name n)
  (if (list? n)
    (if (car n)
      (string-join (map x->string (append (car n)
                                          (if (> (length n) 2)
                                            (list (ref n 2))
                                            (list))))
                   ":")
      "unknown" ;; todo
      )
    n))

(define (stats-by-source all)
  (sort (map (lambda(l)
               (let1 times (map (lambda(x)
                                  (car (assoc-ref x 'runtime)))
                                l)
                 `((source ,(profile->node (car l)))
                   (count ,(size-of times))
                   (total-time ,(apply + times))
                   (min-time ,(apply min times))
                   (max-time ,(apply max times))
                   (avg-time ,(apply avg times)))))
             (group-collection (filter (lambda(x) (car (assoc-ref x 'runtime '(#f))))
                                       all)
                               :key profile->node
                               :test equal?))
        (lambda(x y) (> (car (assoc-ref x 'total-time))
                        (car (assoc-ref y 'total-time))))))

(define (print-totals all)
  (for-each (lambda(x)
              (print (node-name (car (assoc-ref x 'source)))
                     ": "
                     (filter (lambda(x) (not (equal? (car x) 'source)))
                             x)))
            (stats-by-source all)))

(define (callgraph-edges all)
  (sort ;; doesn't really help?
   (map
    (lambda(x)
      `((source ,(car (assoc-ref (car x) 'source)))
        (target ,(car (assoc-ref (car x) 'target)))
        (total-time ,(apply + (map (lambda(e)
                                     (car (assoc-ref e 'runtime)))
                                   x)))
        (parts ,x)))
    (group-collection (map (lambda(x)
                             `((source ,(car (profile->stack-nodes x)))
                               (target ,(profile->node x))
                               (runtime ,(car (assoc-ref x 'runtime)))))
                           (filter (lambda(x)
                                     (car (assoc-ref x 'runtime '(#f))))
                                   all))
                      :key (lambda(e)
                             (list (car (assoc-ref e 'source))
                                   (car (assoc-ref e 'target))))
                      :test equal?))
   (lambda(x y) (> (car (assoc-ref x 'total-time))
                   (car (assoc-ref y 'total-time))))))

(define (with-output-to-graph-easy x)
  (let* ((p (run-process
             '(sh -c "graph-easy --as dot|dot -Tsvg -Gsize=10,12 -Gratio=auto")
             ;;'(cat)
             :input :pipe :output :pipe))
         (r (with-output-to-port (process-input p) x))) ;; todo: might block
    (close-output-port (process-input p))
    (copy-port (process-output p) (current-output-port))
    (process-wait p)
    r))

(define (print-graph all)
  (let* ((all-node-stats (stats-by-source all))
         (node-stats (lambda(n)
                       (find (lambda(x)
                               (equal? (car (assoc-ref x 'source))
                                       n))
                             all-node-stats)))
         (node-time (lambda(n)
                      (if-let1 ns (node-stats n)
                        (car (assoc-ref ns 'total-time '(#f)))
                        #f)))
         (node->string (lambda(n)
                         (string-append "[ " (node-name n)
                                        "\\n"
                                        (if-let1 ns (node-stats n)
                                          (string-append "("
                                                         (string-join
                                                          (list
                                                           #`"cnt=,(car (assoc-ref ns 'count))"
                                                           #`"t=,(runtime->string (car (assoc-ref ns 'total-time)))")
                                                          ",")
                                                         ")")
                                          "" ; todo
                                          )
                                        " ]"))))
    (with-output-to-graph-easy
     (lambda()
       (print "graph{flow:south}")
       (for-each (lambda(edge)
                   (let ((et (car (assoc-ref edge 'total-time))))
                     (print (node->string (car (assoc-ref edge 'source)))
                            " --> { label: \" " ;; note: additional whitespace to make graphviz happy
                            ;; todo: escape
                            (runtime->string et)
                            (if-let1 nt (node-time (car (assoc-ref edge 'source)))
                              #`"(,(round->exact (* (/ et nt) 100))%)"
                              "")
                            "\"; } "
                            (node->string (car (assoc-ref edge 'target))))))
                 (callgraph-edges all))))))

(define (main args)
  (let ((all '())
        (start-time (current-time))
        (update-interval #f))
    (while (read) (compose not eof-object?) => l
           (push! all l)
           (when update-interval
             (let1 end-time (current-time)
               (when (> (time->seconds (time-difference end-time start-time)) update-interval)
                 (set! start-time end-time)
                 (clear-terminal)
                 ;; todo: might be slower than update-interval ;-)
                 (print-totals all)
                 (print-graph all)))))
    ;; (write all)
    ;; (newline)
    (print-totals all)
    (print-graph all)
    )
  0)
