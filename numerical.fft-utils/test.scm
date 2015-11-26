;;;
;;;
(use srfi-1)
(use math.const)
(use ggc.numerical.fftn)
(use ggc.numerical.fft-utils)

(define (compare f1 f2)
  (apply + (map (lambda (x1 x2)
                  (expt (magnitude (- x1 x2)) 2))
                f1 f2)))

(define (gen-cos O A P wfn nfft)
  (map (lambda (n)
         (+ O (* A (cos (- (/ (* 2 pi wfn n) nfft) P)))))
       (iota nfft)))

(define (make-dist c2 c3 c4 c5)
  (lambda (f)
    (+ f
       (* c2 (expt f 2))
       (* c3 (expt f 3))
       (* c4 (expt f 4))
       (* c5 (expt f 5)))))

(define (test1 wfn nfft O A P)
  (let* ((f0 (gen-cos O A  P wfn nfft))
         (fc (fftn-fft  f0))
         (f1 (fftn-rfft fc)))
    (receive (O A P) (fft->OAP fc wfn nfft)
      (print #"O=~|O| A=~|A| P=~|P|")
      (let ((f2 (fft-make-wave O A P wfn nfft)))
        (print #"square sum of (f0-f1)=~(compare f0 f1)")
        (print #"square sum of (f0-f2)=~(compare f0 f2)")
        ))))


(define (test2 wfn nfft c2 c3 c4 c5)

  (let* ((f0  (gen-cos 0 1.0 0 wfn nfft))
         (dis (make-dist c2 c3 c4 c5))
         (f1  (map dis f0))
         (fc  (fftn-fft f1)))

    (receive (sndr enob sfdr fns) (fft->SNDR fc wfn nfft)
      (print #"sndr=~|sndr| enob=~|enob| sfdr=~|sfdr|"))

    (receive (f1 f2 f3 f4 f5 fo no) (fft-harmonics fc wfn nfft)
      (define (db20 x) (* 20 (/ (log (magnitude (/ x f1))) (log 10))))
      (print #"#1 H2,3,4,5,o=~(db20 f2),~(db20 f3),~(db20 f4),~(db20 f5),~(db20 fo) \
               (~(modulo (* no wfn) nfft)/~|nfft|,~|no|th)"))

    (receive (f1 f2 f3 f4 f5 fo no) (fft-harmonics-2 fc wfn nfft)
      (define (db20 x) (* 20 (/ (log (magnitude (/ x f1))) (log 10))))
      (print #"#2 H2,3,4,5,o=~(db20 f2),~(db20 f3),~(db20 f4),~(db20 f5),~(db20 fo) \
               (~|no|/~|nfft|)"))
    (let ()
      (define (db20 x) (* 20 (/ (log x) (log 10))))
      (print #"P   c2/2,c3/4=~(db20 (/ c2 2)),~(db20 (/ c3 4))")
      )))

(test1 15 128 1.0 1.0 1.0)
(test2 15 128 0.0001 0.001 0 0)

;;; EOF
