#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../serialization -I../fork-process -I../runtime-compile -- "$0" "$@"
(use gauche.test)
(use gauche.time)
(use gauche.process)
(use srfi-1)
(use gauche.selector)
(use util.stream)

(test-start "parallel")

(use parallel)
(test-module 'parallel)

(define (fake-slow proc . args)
  (let-optionals* args ((t 0.2e9))
    (lambda l
      (sys-nanosleep t)
      (apply proc l))))

(define (fac x)
  (if (zero? x)
    1
    (* x (fac (- x 1)))))

(define (middle lo hi)
  (/. (+. lo hi) 2))

(define (bisect step lo hi . args)
  (let-optionals* args ((ret #f))
    (cond [(< (- hi lo) 1000)
           ret]
          [else
           (let* ((mid (middle lo hi))
                  (ret+1 (step mid)))
             (if ret+1
               (bisect step (+ mid 1)    hi  ret+1)
               (bisect step        lo   mid    ret)))])))

(define (timed thunk)
  (let1 t (make <real-time-counter>)
    (with-time-counter t
      (thunk))
    (time-counter-value t)))

(let* ((f (fake-slow (cut * 2 <>)))
       (l (list-tabulate (* 8 2) values))
       (r (map f l))
       (ten (cut f 5)))

  (test* "simple async call"
         10
         (bg-call-wait (bg-call ten)))
  
  (test* "handshake"
         10
         (bg-call-wait (bg-call ten
                                :handshake #t)))
  
  (test* "select"
         10
         (let ((b (bg-call ten))
               (selector (make <selector>))
               (res #f))
           (selector-add! selector
                          (bg-call-selectable b)
                          (lambda(p f)
                            (when (not (bg-call-ready? b))
                              (error "not ready"))
                            (set! res (bg-call-wait b)))
                          '(r))
           (and (= (selector-select selector) 1)
                res)))

  (test* "error serialization"
         "foo"
         (guard (e [else (ref e 'message)])
                (bg-call-wait (bg-call (lambda() (error "foo"))))))

  (test* "parallel-map"
         r
         (parallel-map f l))

  ;; note: fragile
  (test* "time"
         #t
         (> (timed (cut map f l))
            (* 2. (timed (cut parallel-map f l)))))

  ;; guess number of cpus
  ;; (likely overestimated)
  (let1 cpus (caar (sort (map (lambda(limit)
                                (list limit
                                      (timed (lambda()
                                               (parallel-map (lambda(x) (fac (- 4000 x)))
                                                             l
                                                             :limit limit
                                                             :lookahead 1000)))))
                              (iota 8 1))
                         (lambda(a b)
                           (< (cadr a) (cadr b)))))
    ;; try to find minimum task time to compensate for overhead
    ;; i get ~68ms with that minimal i/o overhead task !
    #?=cpus
    #?=(/. (bisect (lambda(t)
                     (let1 f (fake-slow (cut * 2 <>) t)
                       (if (> (timed (lambda() (parallel-map f l
                                                             :limit cpus
                                                             :lookahead 1000
                                                             :handshake #f)))
                              (timed (lambda() (map f l))))
                         t
                         #f)))
                   0
                   1e9)
           1e9))

  ;; todo
  ;; (test* "undefined(1)"
  ;;        #t
  ;;        (undefined?
  ;;         #?=(bg-call-wait (bg-call (lambda _)))))

  ;; todo
  ;; (test* "undefined(2)"
  ;;        #t
  ;;        (every undefined?
  ;;               (parallel-map (lambda _) '(1 2 3))))

  (test* "output to closed stderr"
         1
	 (begin
	   (guard (e
		   [else
		    #?=e
                    1])
		  (bg-call-wait
		   (bg-call
		    (lambda()
		      (close-output-port (current-error-port))
		      #?=10
		      (with-output-to-port (current-error-port)
			(lambda()
			  (print "lost")
			  (flush)))
		      1))))))

  (test* "parent output not fd 1"
	 #t
	 (with-output-to-file "/dev/null"
	   (lambda()
	     (bg-call-wait
	      (bg-call
	       (lambda()
		 #t))))))

  ;; todo
  (test "signals"
        r
        (lambda()
          (set-signal-handler! SIGHUP #f) ;; ignore sighup
          (let* ((pid (sys-getpid))
                 (future (bg-call
                          (lambda()
                            (sys-nanosleep 0.05e9)
                            (sys-kill pid SIGHUP)
                            (sys-kill (sys-getpid) SIGHUP)
                            ;;(sys-kill pid SIGPIPE)
                            ;;(sys-kill (sys-getpid) SIGPIPE)
                            (sys-nanosleep 0.05e9)
                            #t))))
            (let1 r (parallel-map f l :limit 8)
              (bg-call-wait future)
              r)))))

(test  "zombies(1)?" '() process-list)
(set-signal-handler! SIGPIPE #f)
(test* "zombies(2)?" '()
       (begin
         (guard (e
                 [else
                  ;;#?=e
                  #t])
                (stream->list (parallel-stream-map (lambda(x)
                                                     (error "foo"))
                                                   (list->stream (iota 4))
                                                   :handshake #f)))
         (process-list)))

(test-end)
