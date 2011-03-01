;;; stream.scm -- from SICP.
;;; 
(define-module ggc.math.prime
  (export prime?
  )
 )

(select-module ggc.math.prime)


(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define the-empty-stream '())

(define stream-null? null?)

(define stream-car car)

(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ 1 low) high))))

(define (stream-map proc . s)
  (if (stream-null? (car s))
      the-empty-stream
      (cons-stream (apply proc (map stream-car s))
		   (apply stream-map proc (map stream-cdr s)))))

(define (stream-for-each proc . s)
  (if (stream-null? (car s))
      'done
      (begin (apply proc (map stream-car s))
	     (apply stream-for-each proc (map stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
	((pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred
				     (stream-cdr s))))
	(else
	 (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each (lambda (x) (display x) (newline)) s))

(define (head-stream s n)
  (if (<= n 0)
      'done
      (begin
	(display (stream-car s)) (newline)
	(head-stream (stream-cdr s) (- n 1)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream s f)
  (stream-map (lambda (x) (* x f)) s))

(define p5.53  (cons-stream 1 (add-streams p5.53 p5.53)))

(define (merge-streams s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge-streams (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge-streams s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge-streams (stream-cdr s1)
					      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge-streams (scale-stream S 2)
					(merge-streams (scale-stream S 3)
						       (scale-stream S 5)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define natural-numbers (integers-starting-from 1))

(define factorials (cons-stream 1 
				(mul-streams natural-numbers
					     factorials)))
(define (partial-sums s)
  (define sums (cons-stream (stream-car s)
			    (add-streams (stream-cdr s) sums)))
  sums)

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve s)
  (cons-stream
   (stream-car s)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car s))))
	   (stream-cdr s)))))

(define sieve-primes
  (sieve (integers-starting-from 2)))

(define stream-primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (square x) (* x x))

(define primes stream-primes)

(define (prime? n)
  (let iter ((ps primes))
    (cond ((> (square (stream-car ps)) n) #t)
	  ((divisible? n (stream-car ps)) #f)
	  (else 
	   (iter (stream-cdr ps))))))

(define (expand1 num den radix)
  (cons-stream 
   (quotient (* num radix) den)
   (expand1 (remainder (* num radix) den) den radix)))

(define (expand2 num den radix)
  (cons-stream 
   (quotient num den)
   (expand2 (* radix (remainder num den)) den radix)))

(provide "ggc/math/prime")
;; EOF
