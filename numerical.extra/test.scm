;;
;; sample test file
;;

(require "extra")
(import ggc.numerical.extra)

(format #t "%erf(0.0) = ~a~%" (%erf 0.0))
(format #t "%erfc(0) = ~a~%" (%erfc 0.0))
(format #t "%erf(1/sqrt(2)) = ~a~%" (%erf (/ 1.0 (sqrt 2.0))))
(format #t "%erf(2/sqrt(2)) = ~a~%" (%erf (/ 2.0 (sqrt 2.0))))
(format #t "%erf(3/sqrt(2)) = ~a~%" (%erf (/ 3.0 (sqrt 2.0))))

(format #t "erf(1) = ~a~%" (erf 1))
(format #t "erf(2) = ~a~%" (erf 2))
(format #t "erf(3) = ~a~%" (erf 3))
(format #t "erf(1/2) = ~a~%" (erf 1/2))
(format #t "erf(1/3) = ~a~%" (erf 1/3))

(define (error-catcher e)
  (format #t "Got error: '~a' as expected~%"
	  (slot-ref e 'message)))

(with-error-handler
 error-catcher
 (lambda ()
   (format #t "erf(i) = ~a~%" (erf (sqrt -1)))))

(with-error-handler
 error-catcher
 (lambda ()
   (format #t "%erf(1) = ~a~%" (%erf 1))))


(define (variance-of-grand N)
  (define (square x) (* x x))
  (let ((variance 0)
	(sum      0))
    (do ((i 0 (+ i 1)))
	((= i N) (format #t "grand (~a samples) mean=~a variance=~a~%"
			 N
			 (/ sum N)
			 (/ variance N)))
      (let ((x (%grand)))
	(set! sum      (+ sum x))
	(set! variance (+ variance (square x)))))))

(variance-of-grand 5)
(variance-of-grand 50)
(variance-of-grand 500)
(variance-of-grand 5000)
(variance-of-grand 50000)

(define (write-bar n)
  (do ((i 0 (+ i 1)))
      ((= i n)  (display "*") (newline))
    (display "+")))

(let ((N 20)
      (offset 10))
  (let ((v (make-vector N 0.0)))
    (do ((i 0 (+ i 1)))
	((= i 40000) #t)
      (letrec ((gau (%grand))
	       (j (floor->exact (+ offset  (* 3 gau)))))
	(and (>= j 0) (< j N)
	     (vector-set! v j (+ 0.01 (vector-ref v j))))))
    (do ((i 0 (+ i 1)))
	((= i N) #t)
      (write-bar (floor->exact (vector-ref v i))))))

