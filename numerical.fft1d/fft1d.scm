(define-module ggc.numerical.fft1d
  (use srfi-1)
  (use srfi-13)
  (use gauche.process)
  (export fft1d-fft fft1d-rfft fft1d-rfft1 fft1d->SNDR)
  )
(select-module ggc.numerical.fft1d)

(define *tempf-tmpl* "/tmp/ggc")
(define (with-temporary-file writer reader)
  (receive (port file) (sys-mkstemp *tempf-tmpl*)
    (dynamic-wind
     (lambda () #f)
     (lambda () 
       (writer port)
       (close-output-port port)
       (reader file))
     (lambda ()
       (if (not (port-closed? port))
           (close-output-port port))
       (sys-unlink file)))))

;;;
;;;  FFT by fft1d
;;;
(define (str->z str)
  (with-input-from-string str
    (lambda ()
      (let ((real (read))
            (imag (read)))
        (make-rectangular real imag)))))

(define (fft1d-fft lis)
  (with-temporary-file
   (lambda (p) 
     (with-output-to-port p
       (lambda () (for-each print lis))))
   (lambda (tmpf)
     (let ((proc (string-append "fft1d < " tmpf)))
       (with-input-from-process proc
	 (lambda ()			       
	   (port-map str->z read-line)))))))

(define (fft1d-rfft fc)
  (with-temporary-file
   (lambda (p) 
     (for-each (lambda (z) 
                 (format p "~a ~a~%" (real-part z) (imag-part z)))
               fc))
   (lambda (tmpf)
     (let ((proc (string-append "rfft1d < " tmpf)))
       (with-input-from-process proc
	 (lambda ()			       
	   (port-map str->z read-line)))))))

(define (fft1d-rfft1 fc wfn nfft)
  (let ((vec (make-vector nfft 0))
	(wfa (modulo wfn nfft))
	(wfb (modulo (- wfn) nfft)))
    (vector-set! vec 0   (list-ref fc 0))
    (vector-set! vec wfa (list-ref fc wfa))
    (vector-set! vec wfb (list-ref fc wfb))
    (fft1d-rfft (vector->list vec))))

(define (fft1d->SNDR fc wfn nfft)
  (with-temporary-file
   (lambda (p)
     (for-each (lambda (z)
                 (format p "~a ~a ~a~%" 
			 (expt (abs z) 2) (real-part z) (imag-part z)))
               fc))
   (lambda (tmpf)
     (let* ((proc (string-append "sndr < " tmpf))
            (ans (string-tokenize (with-input-from-process proc read-line)))
            ;;   0     1                  2     3   4     5   6     7
            ;; "sndr:  4.6603343592e+08   86.68 dB  14.11 bit SFDR= 90.57 dB"
            (SNDR (list-ref ans 2))
            (ENOB (list-ref ans 4))
            (SFDR (list-ref ans 7)))
       (values SNDR ENOB SFDR)))))

(provide "ggc/numerical/fft1d")
;;; EOF
