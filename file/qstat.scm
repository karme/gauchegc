;;; $Id: qstat.scm,v 1.3 2004/03/30 01:37:57 skimu Exp $

;;;  NQS's qstat reader.

(define-module ggc.file.qstat
  (use srfi-1)     ; List library
  (use srfi-13)    ; String Library
  (export getq simp))
(select-module ggc.file.qstat)


(define *d* #f)

(define (m  x) 
  (if *d*
      (display x (current-error-port))))
(define (mn x) 
  (if *d*
      (begin
       (display x (current-error-port)) 
       (newline (current-error-port)))))

(define (getq)
  (define (qname s)
    (car (string-tokenize s)))
  (let lp ((l (read-line))
	   (q '()))
    (m l)
    (if (eof-object? l) 
	q
	(if (string-scan l "type=BATCH;")
	    (begin
	      (mn "*** Picked")
	      (read-line) 
	      (read-line)
	      (let ((u (getu)))
		(lp (read-line) 
		    (cons (cons (qname l) u)
			  q))))
	    (begin
	      (mn "***")
	      (lp (read-line) q))))))

	
(define (getu)
  (define (uname l) (list-ref (reverse l) 1))
  (let lp ((l (read-line))
	   (u '()))
    (if (eof-object? l) 
	u
	(let ((x (string-tokenize l)))
	  (cond ((null? x) u)
		((string=? "REQUEST" (car x)) (lp (read-line) u))
		((>= (length x) 5)
		 (lp (read-line)
		     (cons (uname x) u)))
		(else u))))))
		     
(define (simp l)
  (define (count l)
    (if (null? l) '()
	(let lp ((u (car l))
		 (c 1)
		 (l l) 
		 (r '()))
	  (cond ((null? l) (cons (cons u c) r))
		((string=? u (car l)) 
		 (lp u (+ c 1) (cdr l) r))
		(else 
		 (lp (car l) 1 (cdr l) (cons (cons u c) r)))))))
  (cons (car l)
	(count (sort (cdr l)))))

(provide "ggc/file/qstat")

;;; EOF
