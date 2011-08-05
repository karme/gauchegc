#!/usr/bin/gosh -I.
(use balance-board)
(use util.list)

(define (weights->xy weights)
  (let* ((wc (map (lambda(x)
		    (cons (car x) (max 0.0 (cdr x))))
		  weights))
	 (t (apply + (map cdr wc)))
	 (wn (map (lambda(x)
		    (cons (car x) (/. (cdr x) t)))
		  wc)))
    (if (<= t 0)
	'(0.5 0.5)
	(list
	 (+.
	  (assoc-ref wn 'right_top)
	  (assoc-ref wn 'right_bottom))
	 (+.
	  (assoc-ref wn 'left_top)
	  (assoc-ref wn 'right_top))))))

(define (main args)
  (let1 b (balance-open)
	(unwind-protect
	 (begin
	   (dotimes (i 20)
		    (sys-nanosleep 0.5e9)
		    (let1 w (balance-weights b)
			  (print (round->exact (/. (apply + (map cdr w)) 1000))
				 "kg "
				 (weights->xy w))))
	   0)
	 (balance-close b))))

