;; Gauche Garbage Collection ggc.numerical.fftn.
;; written by skimu.

(define-module ggc.numerical.fftn
  (use gauche.uvector)
  (export fftn       fft-free
          fftn-fft   fftn-rfft
          fftn-fftv  fftn-rfftv
          fftn-fftv! fftn-rfftv! )
  (dynamic-load "ggcnumfftn")
  )
(select-module ggc.numerical.fftn)

(define (fftn-fft  lis)  (fftn-fft1 lis  1 -1.0))
(define (fftn-rfft lis)  (fftn-fft1 lis -1  1.0))

(define (fftn-fft1 lis iSign Scale)
  (let* ((r  (map real-part lis))
         (i  (map imag-part lis))
         (rv (list->f64vector r))
         (iv (list->f64vector i))
         (n  (f64vector-length rv)))
    (fftn (list n) rv iv iSign Scale)
    (map make-rectangular
         (f64vector->list rv)
         (f64vector->list iv))))
#|
(define (fftn-fftv   vec) (fftn-fft1v  vec  1 -1.0))
(define (fftn-rfftv  vec) (fftn-fft1v  vec -1  1.0))
(define (fftn-fftv!  vec) (fftn-fft1v! vec  1 -1.0))
(define (fftn-rfftv! vec) (fftn-fft1v! vec -1  1.0))

(define (fftn-fft1v vec iSign Scale)
  (let* ((n (vector-length vec))
         (r (make-f64vector n))
         (i (make-f64vector n))
         (v (make-vector n))) ; result
    (do ((l 0 (+ l 1)))
        ((= l n) #t)
      (f64vector-set! r l (real-part (vector-ref vec l)))
      (f64vector-set! i l (imag-part (vector-ref vec l))))
    (fftn (list n) r i iSign Scale)
    (do ((l 0 (+ l 1)))
        ((= l n) v)
      (vector-set! v l (make-rectangular
                        (f64vector-ref r l)
                        (f64vector-ref i l))))))

(define (fftn-fft1v! vec iSign Scale)
  (let* ((n (vector-length vec))
         (r (make-f64vector n))
         (i (make-f64vector n)))
    (do ((l 0 (+ l 1)))
        ((= l n) #t)
      (f64vector-set! r l (real-part (vector-ref vec l)))
      (f64vector-set! i l (imag-part (vector-ref vec l))))
    (fftn (list n) r i iSign Scale)
    (do ((l 0 (+ l 1)))
        ((= l n) vec)
      (vector-set! vec l (make-rectangular
                          (f64vector-ref r l)
                          (f64vector-ref i l))))))
|#

(provide "ggc/numerical/fftn")

