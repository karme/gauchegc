(define-module ggc.numerical.fft-utils
  (use srfi-1)
  (use srfi-13)
  (use math.const)
  (export-all)
  )
(select-module ggc.numerical.fft-utils)

(define (phase->time phase wfn nfft fs)
  ;; cos(wt + P) = cos(w(t - t0)), 
  ;;    --> t0 = -P/w
  (cond ((> phase pi)
	 (phase->time (- phase (* 2 pi)) wfn nfft fs))
	((< phase (- pi))
	 (phase->time (+ phase (* 2 pi)) wfn nfft fs))
	(else
	 (let ((w (/ (* 2 pi wfn fs) nfft)))
	   (- (/ phase w))))))

(define (fft->OAP fc wfn nfft)
  (let ((f00 (list-ref fc 0))
	(f01 (list-ref fc (modulo wfn nfft))))
    ;; assuming fft of real function
    (let ((O (magnitude f00))          ;; Offset
	  (A (* 2 (magnitude f01)))    ;; Amplitude
	  (P (angle f01)))       ;; Phase
      (values O A P))))

(define (fft-P->npf-old P wfn nfft)
  ;; Normalized phase funtion
  (lambda (n)
    (let lp ((P (- (/ (* 2 pi wfn n) nfft) P)))
      (cond ((< P 0)        (lp (+ P (* 2 pi))))
	    ((< (* 2 pi) P) (lp (- P (* 2 pi))))
	    (else P)))))

(define (fft-P->npf P wfn nfft)
  (lambda (n)
    (let ((P (- (/ (* 2 pi wfn n) nfft) P)))
      (mod P (* 2 pi)))))

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
  (let ((f (fft-OAP->wf O A P wfn nfft)))
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
    (format #t "## ZZ* Real Imag Harmonics (WFN=~a, NFFT=~a)~%" wfn nfft)
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
      (fft-dump-with-harmonics-index fc wfn nfft))))
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

(define (fft-harmonics fc wfn nfft)
  ;; no is harmonic number of fo
  (let* ((uc  (fft/unwrap fc wfn nfft))
         (f0  (car   uc)) (uc  (cdr   uc))
         (f1  (car   uc)) (uc  (cdr   uc))
         (f2  (car   uc)) (uc  (cdr   uc))
         (f3  (car   uc)) (uc  (cdr   uc))
         (f4  (car   uc)) (uc  (cdr   uc))
         (f5  (car   uc)) (uc  (cdr   uc)))
    (let lp ((n 6) (fo 0) (no 0) (uc uc))
      (cond ((null? fc) (error "fft-harmonics: something went wrong"))
            ((>= n (/ nfft 2)) (values f1 f2 f3 f4 f5 fo no))
	    ((> (magnitude (car uc))
                (magnitude fo))
	     (lp (+ n 1) (car uc) n (cdr uc)))
	    (else (lp (+ n 1) fo no (cdr uc)))))))

(define (fft-harmonics-2 fc wfn nfft)
  ;; no is wave number (frequency) of fo
  (define (mth? m n) (= n (modulo (* m wfn) nfft)))
  (let lp ((fc (cdr fc))
           (n  1)
           (f1 0) (f2 0) (f3 0) (f4 0) (f5 0) (fo 0) (no 0))
    (cond ((null? fc) (values f1 f2 f3 f4 f5 fo no))
          ((mth? 1 n) (lp (cdr fc) (+ n 1) (car fc) f2 f3 f4 f5 fo no))
          ((mth? 2 n) (lp (cdr fc) (+ n 1) f1 (car fc) f3 f4 f5 fo no))
          ((mth? 3 n) (lp (cdr fc) (+ n 1) f1 f2 (car fc) f4 f5 fo no))
          ((mth? 4 n) (lp (cdr fc) (+ n 1) f1 f2 f3 (car fc) f5 fo no))
          ((mth? 5 n) (lp (cdr fc) (+ n 1) f1 f2 f3 f4 (car fc) fo no))
          ((or (mth? -1 n) (mth? -2 n) (mth? -3 n) (mth? -4 n) (mth? -5 n))
           (lp (cdr fc) (+ n 1) f1 f2 f3 f4 f5 fo no))
          (else
           (if (> (magnitude (car fc))
                  (magnitude fo))
               (lp (cdr fc) (+ n 1) f1 f2 f3 f4 f5 (car fc) n)
               (lp (cdr fc) (+ n 1) f1 f2 f3 f4 f5 fo no))))))

(provide "ggc/numerical/fft-utils")
