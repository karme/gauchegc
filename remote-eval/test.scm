#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../serialization -- "$0" "$@"
;; very similar to bg-call
(use serialization)
(use gauche.process)

(define (make-evaluator :key
                        (host #f))
  ;; todo: define serialization module on the fly
  (let1 p (run-process '(gosh -I/home/karme/develop/scheme/gauchegc/serialization -userialization -b) :host host :input :pipe :output :pipe)
    (with-output-to-port (process-input p)
      (lambda()
        ;; todo:
        ;; really use read-eval-print-loop
        ;; s.a.
        ;; gauche's libeval.scm
        (write '(read-eval-print-loop
                 #f
                 ;; evaluator
                 (lambda(expr env)
                   (guard (e [(<error> e)
                            ;; todo:
                            ;; - loosing too much information, here
                            ;; - at least provide a stack trace
                            ;; - how preserve error subclass?
                              (make <serializable-error> :message (ref e 'message))]
                             [else
                              (make <serializable-error> :message "unknown error")])
                          (receive l (eval expr env)
                            l)))
                 ;; printer
                 (lambda l
                   (apply serialize l)
                   (flush))
                 ;; prompter
                 (lambda _ )))
        (flush)))
    `((eval . ,(lambda(expr)
                 (with-ports (process-output p) (process-input p) #f
                             (lambda()
                               (write expr)
                               (newline)
                               (deserialize-result))))))))

;; (define e (make-evaluator :host "prisirah@green.local"))

(define (writeln x)
  (write x)
  (newline))

(define (main args)
  (let1 e (make-evaluator)
    (writeln ((assoc-ref e 'eval) '(+ 1 2 3))))
  (let1 e (make-evaluator :host "localhost")
    (writeln ((assoc-ref e 'eval) '(+ 1 2 3))))
  (let1 e (make-evaluator :host "localhost")
    (writeln ((assoc-ref e 'eval) 'kkk)))
  0)
