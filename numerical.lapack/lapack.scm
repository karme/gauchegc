(define-module ggc.numerical.lapack
  (use gauche.uvector)
  (export-all)
  )
(select-module ggc.numerical.lapack)

(dynamic-load "ggcnumlapack")

;; This should go somewhere else, but for now...
;;;
;;; LEAST SQUARE FITTING TO N-th ORDER POLYNOMIAL
;;;
(define (polyNfit N xlis ylis)
  (let* ((N (+ N 1))
         (X (list->f64vector xlis))
         (Y (list->f64vector ylis))
         (NPOINTS (f64vector-length Y))
         (A (make-f64vector (* NPOINTS N)))
         (LW (* N 128))
         (W (make-f64vector LW)))

    (do ((i 0 (+ i 1)))
        ((= i NPOINTS) #t)
      (do ((n 0  (+ n 1))
           (xx 1 (* xx (f64vector-ref X i))))
          ((= n N) #t)
        (f64vector-set! A (+ (* n NPOINTS) i) xx)))

    (let ((info (dgels #\N NPOINTS N 1 A NPOINTS Y NPOINTS W LW)))
      (if (= info 0)
          (f64vector->list Y 0 N)
          (errorf "DGELS returned non-zero INFO (~a)" info)))))

(define (poly1fit xlis ylis)  (polyNfit 1 xlis ylis))
(define (poly2fit xlis ylis)  (polyNfit 2 xlis ylis))
(define (poly3fit xlis ylis)  (polyNfit 3 xlis ylis))
(define (poly4fit xlis ylis)  (polyNfit 4 xlis ylis))

(define (make-z64vector x)   (make-f64vector (* x 2)))
(define (z64vector-length x)
  (let ((n (/ (f64vector-length x) 2)))
    (if (not (integer? n))
      (error "something is wrong"))
    n))

(define (list->z64vector lis)
  (list->f64vector (append-map (lambda (x)
                                (list (real-part x)
                                      (imag-part x)))
                              lis)))

(define (z64vector->list v :optional (start 0) (end (z64-vector-length v)))
  (if (>= start end) (error "invalid start and end" start end))
  (let lp ((i start) (r '()))
    (if (= i end)
      (reverse r)
      (lp (+ i 1)
          (cons (z64vector-ref v i) r)))))

(define (z64vector-ref v i)
  (let ((n (* 2 i)))
    (make-rectangular (f64vector-ref v n)
                      (f64vector-ref v (+ n 1)))))

(define (z64vector-set! v i x)
  (let ((n (* 2 i)))
    (f64vector-set! v n (real-part x))
    (f64vector-set! v (+ n 1) (imag-part x))))

;;;
;;;
(define (zpolyNfit N xlis ylis)
  (let* ((N (+ N 1))
         (X (list->z64vector xlis))
         (Y (list->z64vector ylis))
         (NPOINTS (z64vector-length Y))
         (A (make-z64vector (* NPOINTS N)))
         (LW (* N 128))
         (W (make-z64vector LW)))

    (do ((i 0 (+ i 1)))
        ((= i NPOINTS) #t)
      (do ((n 0  (+ n 1))
           (xx 1 (* xx (z64vector-ref X i))))
          ((= n N) #t)
        (z64vector-set! A (+ (* n NPOINTS) i) xx)))

    (let ((info (zgels #\N NPOINTS N 1 A NPOINTS Y NPOINTS W LW)))
      (if (= info 0)
          (z64vector->list Y 0 N)
          (errorf "ZGELS returned non-zero INFO (~a)" info)))))

;;;
;;;  A list of real, NxN elements.
;;;  B list of real, N elements.
;;;
(define (call-dgesv lisA lisB)
  (let* ((A    (list->f64vector lisA))
         (B    (list->f64vector lisB))
         (N    (f64vector-length B))
         (IPIV (make-s32vector N 0)))
    (if (not (= (f64vector-length A) (* N N)))
      (error "invalid input."))
    (let ((info (dgesv N 1 A N IPIV B N)))
      (if (= info 0)
        (values (f64vector->list B)
                (s32vector->list IPIV))
        (errorf "DGESV returned non-zero INFO (~a)" info)))))

;;;
;;;  A list of complex, NxN elements.
;;;  B list of complex, N elements.
;;;
(define (call-dgesv lisA lisB)
  (let* ((A    (list->z64vector lisA))
         (B    (list->z64vector lisB))
         (N    (z64vector-length B))
         (IPIV (make-s32vector N 0)))
    (if (not (= (z64vector-length A) (* N N)))
      (error "invalid input."))
    (let ((info (zgesv N 1 A N IPIV B N)))
      (if (= info 0)
        (values (z64vector->list B)
                (s32vector->list IPIV))
        (errorf "ZGESV returned non-zero INFO (~a)" info)))))

(provide "ggc/numerical/lapack")
