(define-module balance-board
  (use c-wrapper)
  (use gauche.collection)
  (use util.list)
  (export balance-open
	  balance-weights
	  balance-close))

(select-module balance-board)

(c-load "cwiid_clean_subset.h"
        :cflags "-I."
        :libs-cmd "sh -c 'echo -L/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH) $(pkg-config --libs cwiid)'")

(define BDADDR_ANY (strtoba "00:00:00:00:00:00"))

(define (balance-open)
  (let1 r `((wii   . ,(cwiid_open BDADDR_ANY 0))
	    (state . ,(make <c-struct:cwiid_state>))
	    (cal   . ,(make <c-struct:balance_cal>)))
	(when (null-ptr? (assoc-ref r 'wii))
	      (error "open failed")) ;; todo: improve error handling
	(cwiid_set_rpt_mode (assoc-ref r 'wii)
			    (logior CWIID_RPT_STATUS
				    CWIID_RPT_BALANCE))
	(let loop ((try 0))
	  (cwiid_get_state (assoc-ref r 'wii)
			   (ptr (assoc-ref r 'state)))
	  (when (not (= (ref (assoc-ref r 'state) 'ext_type)
			CWIID_EXT_BALANCE))
		(cond [(< try 10)
		       (sys-nanosleep 0.5e9)
		       (loop (+ try 1))]
		      [else
		       (error "not a balance board or buggy libcwiid")])))
	(cwiid_get_balance_cal (assoc-ref r 'wii)
			       (ptr (assoc-ref r 'cal)))
	r))

(define (weights reading balance-cal)
  (map (lambda(x)
	 (let ((r (x->number (ref reading x)))
	       (cal (map x->number (ref balance-cal x))))
	   (cons x
		 (let1 o (if (< r (ref cal 1))
			     0
			     1)
		       (+ (*. 17e3 o)
			  (*. 17e3
			      (/ (- r (ref cal o))
				 (- (ref cal (+ o 1))
				    (ref cal      o)))))))))
       '(right_top right_bottom left_top left_bottom)))

(define (balance-weights handle)
  (when (not (zero? (cwiid_get_state (assoc-ref handle 'wii)
				     (ptr (assoc-ref handle 'state)))))
	(error "failed to get state"))
  (weights (ref* (assoc-ref handle 'state) 'ext 'balance)
	   (assoc-ref handle 'cal)))

(define (balance-close handle)
  (cwiid_close (assoc-ref handle 'wii)))
