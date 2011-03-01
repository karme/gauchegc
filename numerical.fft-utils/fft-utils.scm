(define-module ggc.numerical.fft-utils
  (use srfi-1)
  (use srfi-13)
  (use math.const)
  (export-all)
  )
(select-module ggc.numerical.fft-utils)

(define (fft->OAP fc wfn nfft)
  (let ((f00 (list-ref fc 0))
	(f01 (list-ref fc (modulo wfn nfft))))
    ;; assuming fft of real function
    (let ((O (magnitude f00))          ;; Offset
	  (A (* 2 (magnitude f01)))    ;; Amplitude
	  (P (angle f01)))       ;; Phase
      (values O A P))))

(define (fft-P->npf P wfn nfft)
  ;; Normalized phase funtion
  (lambda (n)
    (let lp ((P (- (/ (* 2 pi wfn n) nfft) P)))
      (cond ((< P 0)        (lp (+ P (* 2 pi))))
	    ((< (* 2 pi) P) (lp (- P (* 2 pi))))
	    (else P)))))

(define (fft-OAP->wf O A P wfn nfft)
  ;; Wave function
  (lambda (n)
    (+ O (* A (cos (- (/ (* 2 pi wfn n) nfft) P))))))

(define (fft->power-spectrum fc)
  (map (lambda (z) (expt (magnitude z) 2)) fc))

(define (fft-unwrap lis wfn nfft)
  ;; Unwrap Transform
  (let ((vec (make-vector nfft 0)))
    (let lp ((n 0)
	     (lis lis))

      (define (next n)
	(modulo (+ n wfn) nfft))

      (cond ((null? lis) 
	     (vector->list vec))
	    (else
	     (vector-set! vec n (car lis))
	     (lp (next n)
		 (cdr lis)))))))

(define (fft/unwrap lis wfn nfft)
  ;; Inverse Unwrap Transform
  (let ((vec (list->vector lis)))
    (let lp ((n 0)
	     (r '()))
      (if (= n nfft) 
	  (reverse r)
	  (lp (+ n 1)
	      (cons (vector-ref vec (modulo (* n wfn) nfft))
		    r))))))

(define (fft-make-wave O A P wfn nfft)
  (let ((f (fft->OAP-wf O A P wfn nfft)))
    (let lp ((i     0)
	     (wave '()))
      (if (= i nfft)
	  (reverse wave)
	  (lp (+ i 1)
	      (cons (f i) wave))))))

(define (fft-dump fc)
  (print "## ZZ* Real Imag")
  (for-each (lambda (z)
              (let* ((r (real-part z))
                     (i (imag-part z))
                     (a (expt (magnitude z) 2)))
                (format #t "~a ~a ~a~%" a  r i)))
            fc))

(define (fft-dump-with-harmonics-index fc wfn nfft)
  (let ((idx (fft-unwrap (iota nfft) wfn nfft)))
    (format #t "## ZZ* Real Imag Harmonics (WFN=~a, NFFT=~a)~" wfn nfft)
    (for-each (lambda (n z)
                (let* ((r (real-part z))
                       (i (imag-part z))
                       (a (expt (magnitude z) 2)))
                  (format #t "~a ~a ~a ~a~%" a  r i n)))
              idx fc)))
(define fft-dump/hi fft-dump-with-harmonics-index)

(define (fft-write-to-file fc file)
  (with-output-to-file file (lambda () (fft-dump fc))))

(define (fft-write-to-file-with-harmonics-index fc wfn nfft file)
  (with-output-to-file file 
    (lambda () 
      (fft-dump-with-harmonics-index fc wfn nfft file))))
(define fft-write-to-file/hi fft-write-to-file-with-harmonics-index)

(define (fft->SNDR fc wfn nfft)

  (define (sndr->enob dB) (/ (- dB 1.76) 6.02))
  (define (dB10 r)  (* 10 (/ (log r) (log 10))))

  (let* ((psv   (list->vector fc))
	 (fn1   (modulo wfn     nfft))
	 (fn2   (modulo (- wfn) nfft))
	 (norm  0)
	 (noise 0)
	 (fs    0)
	 (fns   0))

    ;; convert psv to noise power spectrum 
    ;; and calculate norm at the same time
    (let lp  ((n  0)
	      (no 0))
      (if (= n nfft)
	  (set! norm no)
	  (let* ((z (vector-ref psv n))
		 (a (expt (magnitude z) 2)))
	    (vector-set! psv n a)
	    (lp (+ n 1) (+ no a)))))

    ;; Pick up DC fundamental and alias
    (let ((f0   (vector-ref psv 0))
	  (f1   (vector-ref psv fn1))
	  (f2   (vector-ref psv fn2)))

      (vector-set! psv 0   0)
      (vector-set! psv fn1 0)
      (vector-set! psv fn2 0)
      (set! noise (- norm f0 f1 f2))

      ;; find highest bin
      (let lp ((n   0)
	       (tfs  0)
	       (tfns 0))
	(if (= n nfft)
	    (begin
	      (set! fs   tfs)
	      (set! fns  tfns))
	    (if (< tfs (vector-ref psv n))
		(lp (+ n 1) (vector-ref psv n) n)
		(lp (+ n 1) tfs tfns))))
      ;;
      (let* ((sndr  (dB10 (/ (+ f1 f2) noise)))
	     (enob  (sndr->enob sndr))
	     (sfdr  (dB10 (/ (max f1 f2) fs))))
	(values sndr enob sfdr fns)))))

(provide "ggc/numerical/fft-utils")
