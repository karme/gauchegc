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

(provide "ggc/numerical/lapack")
