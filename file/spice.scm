;;; $Id: spice.scm,v 1.3 2004/06/12 00:11:38 skimu Exp $

;;; SPICE punch file reader and associated tools.


(define-module ggc.file.spice
  (use srfi-13)  ; String library
  (use srfi-14)  ; Character set
  (export pun-get-signal-from-cip
	  pun-get-signal-list-from-cip
	  read-pun
	  read-pun2
	  print-pun
	  pun-signals
	  pun-signal
	  bits2dec
	  binize-signal
	  douts2dec
	  )
  )
(select-module ggc.file.spice)

(define (read-pairs)
  (define (parse-line line)
    (let ((nums (cdr (string-tokenize line))))
      (if (and (not (null? nums))
	       (not (string->number (car nums))))
	  (parse-line (read-line))
	  (let lp ((nums nums) (lis '()))
	    (if (null? nums) 
		lis
		(lp (cddr nums) 
		    (cons (cons 
			   (string->number (car nums))
			   (string->number (cadr nums)))
			  lis)))))))
  (let lp ((chr (peek-char))
	   (lis  '()))
    (cond 
     ((char=? #\+ chr)
      (let ((a (parse-line (read-line))))
	(lp (peek-char) (append a lis))))
     (else
      (reverse lis)))))


(define (pun-get-signal-from-cip name)	; cip = Current Input Port
  (define (skip)
    (let ((n (string-length name)))
      (let lp ((line (read-line)))
	(cond 
	 ((eof-object? line) #f)
	 ((and (> (string-length line) n)
	       (string=? name (substring line 0 n)))
	  #t)
	 (else (lp (read-line)))))))
  (and (skip) 
       (read-pairs)))

(define (pun-get-signal-list-from-cip) ; cip = Current Input Port
  (let lp ((line (read-line))
	   (signals '()))
    (cond 
     ((eof-object? line) signals)
     ((string=? line "") (lp (read-line) signals))
     (else
      (case (string-ref line 0)
	((#\* #\+ #\space) (lp (read-line) signals))
	(else 
	 (lp (read-line) 
	     (cons (car (string-tokenize line))
		   signals))))))))

(define (read-header)
  (define (parse-a-line str)
    (let lp ((tokens (string-tokenize str #[[:alnum:]=/:_]))
	     (lhs '())
	     (als '()))
      (if (null? tokens)
	  als
	  (if (and (string=? (car tokens) "=")
		   (> (length tokens) 1))
	      (lp (cddr tokens) 
		  '()
		  (cons (list lhs (cadr tokens)) als))
	      (lp (cdr tokens) 
		  (car tokens)
		  als)))))
  (let lp ((chr (peek-char))
	   (lis '()))
    (cond
     ((eq? #\* chr)
      (let ((a (parse-a-line (read-line))))
	(lp (peek-char)
	    (append a lis))))
     (else 
      (reverse lis)))))

(define (seek-signal)
  (let lp ((line (read-line)))
    (cond ((string-null? line) #f)
	  ((eof-object? line)  #f)
	  (else
	   (case (string-ref line 0)
	     ((#\* #\+ #\space) (lp (read-line)))
	     (else (string-tokenize line)))))))

(define (read-pun2)
  (let lp ((pun (list (cons 'HEADER (read-header)))))
    (let ((signal (seek-signal)))
      (if signal
	  (lp (cons (cons (string->symbol (car signal))
			  (read-pairs))
		    pun))
	  (reverse pun)))))

(define (read-pun)
  (let ((header (list (cons 'HEADER (read-header))))
	(nodes  (read-fort '())))
    (append! header (map convpun nodes))))

(define (pun-signals pun)
  (map car pun))

(define (pun-signal pun name)
  (let ((x (assq name pun)))
    (if x (cdr x) x)))

(define (print-pun pun)
  (format #t "(~%")
  (for-each (lambda (x)     
	      (format #t "  (~a  ~%" (car x))
	      (for-each (lambda (x)
			  (format #t "    ~a~%" x))
			(cdr x))
	      (format #t "  )~%"))
	    pun)
  (format #t ")~%"))

(define (read-entry res cont?)
  (define (pac str)
    (let ((l (string-tokenize str)))
      (cond
       ((null? l) '())
       (cont?  (cdr l))
       (else l))))
  (let ((l (pac (read-line))))
    (if (and (null? l)
	     (not (eq? #\+ (peek-char))))
	res
	(let ((res (append! res l)))
	  (case (peek-char)
	    ((#\+) (read-entry res #t))
	    (else res))))))

(define (read-fort res)
  (case (peek-char)
    ((#\*) 
     (read-line)
     (read-fort res))
    (else
     (let ((e (read-entry '() #f)))
       (if (null? e)
	   (reverse res)
	   (read-fort (cons e res)))))))

(define (strnums->pairs lis)
  (let lp ((lis lis) (res '()))
    (if (null? lis)
	(reverse res)
	(lp (cddr lis)
	    (cons (cons (string->number (car lis))
			(string->number (cadr lis)))
		  res)))))

(define cs-exp (char-set-adjoin char-set:digit #\+ #\- #\. #\E #\e))
(define (convpun lis)
  (let ((n (string->symbol (car lis))))
    (let lp ((lis (member "PWL" lis)) (res '()))
      (if (or (not lis) (null? lis))
	  (cons n res)
	  (let ((x (car lis)))
	    (cond ((string=? (string-take-right x 1) "=")
		   (lp (cddr lis) res))
		  ((string-every cs-exp x)
		   (lp '() (strnums->pairs lis)))
		  (else (lp (cdr lis) res))))))))

;;;; Utilities section
;;
;;  point
;;
(define make-point cons)
(define point-x    car)
(define point-y    cdr)

;; For two dimentional list
;; l = ((p0 p1 p2 ...)
;;      (q0 q1 q3 ...)
;;      (r0 r1 r3 ...))
;; (car2d l) -> (p0 q0 r0)
;; (cdr2d l) -> ((p1 p2 ...) (q1 q2 ...) (r1 r2 ...))
;;
(define car2d   (lambda (x) (map car x)))
(define cdr2d   (lambda (x) (map cdr x)))
(define end2d?  (lambda (x) (null? (car x))))

(define (bits2dec bits)
  (if (null? bits)
      0
      (if (or (= (car bits) 0)
	      (= (car bits) 1))
	  (+ (car bits) (* 2 (bits2dec (cdr bits))))
	  (error "bits must be 1 or 0"))))

(define (douts2dec lbs)
  (let lp ((dec '())
	   (lbs lbs))
    (if (end2d? lbs)
	(reverse dec)
	(let ((dout (car2d lbs)))
	  (lp (cons (make-point (point-x (car dout))
				(bits2dec (map point-y dout)))
		    dec)
	      (cdr2d lbs))))))

(define (binize-signal th pts)
  (define (binize p)
    (make-point (point-x p)
		(if (> (point-y p) th) 1 0)))
  (map binize pts))

;; (print-pun (with-input-from-file "sim.pun" read-pun))

(provide "ggc/file/spice")
