#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(use srfi-1)
(use util.isomorph)
(use util.list)

(define (serialize-string x)
  (with-output-to-string (cut serialize x)))

(define (deserialize-string x)
  (with-input-from-string x deserialize))

(test-start "serialization")
(use serialization)
(test-module 'serialization)

(define-macro (test-serialize x)
  `(test* (format/ss #f "serialize ~s" ,`',x)
          ,x
          ;;((compose deserialize-string (lambda(x) (debug-print x)) serialize-string) ,x)
          ((compose deserialize-string serialize-string) ,x)
          isomorphic?))

(test-serialize (list 1 2 3))
(test-serialize 10)
(test-serialize 10.9+5i)
(test-serialize #\c)
(test-serialize (circular-list 1 2 3))
(test-serialize #(0 1 2 3))
(test-serialize #0=#(0 1 #0#))
;; todo:
(test* "serialization error"
       (test-error <error>)
       (serialize-string (alist->hash-table '((a 1) (b 2) (c 3)))))

;; fails
;; (test-serialize (make <error> :message "foo"))
(test-serialize (make <serializable-error> :message "foo"))

(define (serialize-result-string x)
  (with-output-to-string (cut serialize-result x)))

(define (deserialize-result-string x)
  (with-input-from-string x deserialize-result))

(define-macro (values->list x)
  `(receive l ,x l))
      
(define-macro (test-serialize-result x)
  `(test* (format/ss #f "serialize-result ~s" ,`',x)
          (values->list ,x)
          ;;((compose deserialize-string (lambda(x) (debug-print x)) serialize-string) ,x)
          (values->list ((compose deserialize-result-string serialize-result-string) (lambda() ,x)))
          isomorphic?))

(test-serialize-result 1)
(test-serialize-result (+ 1 2 3))
(test-serialize-result (values 1 2 3))
(test-serialize-result (values '(1 2 3) '(a b c)))
;; todo: improve
(test* "error serialization"
       (test-error <error>)
       ((compose deserialize-result-string serialize-result-string) (lambda() (error "foo"))))
(test-end)
