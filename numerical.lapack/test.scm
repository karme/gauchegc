(use srfi-1)
(use math.const)
(use ggc.numerical.lapack)

;;(print "## ggc.numerical.lapack has been loaded.")

;;;
;;; POLY: ONE VARIABLE POLYNOMIAL
;;;
(define (make-poly . coef) coef)

(define (poly-order poly)
  (- (length poly) 1))

(define (poly-coef poly) poly)
  
(define (poly->func poly)
  (lambda (x)
    (let lp ((xx 1)
             (f  0)
             (c  poly))
      (if (null? c)
          f
          (lp (* xx x) 
              (+ f (* (car c) xx))
              (cdr c))))))
  
(define (poly-display poly)
  (format #t "~a + ~a * x"
          (car poly) (cadr poly))
  (let lp ((n 2)
           (poly (cddr poly)))
    (if (null? poly)
        #t
        (begin
          (format #t " + ~a * x**~a" (car poly) n)
          (lp (+ n 1) (cdr poly))))))

(define (poly->str poly)
  (with-output-to-string 
    (lambda () 
      (poly-display poly))))

(define (poly2-display-root poly)
  (if (= (poly-order poly) 2)
      (let ((a (list-ref poly 2))
            (b (list-ref poly 1))
            (c (list-ref poly 0)))
        (let ((S (- (expt b 2) (* 4 a c))))
          (if (< S 0)
              (poly-display poly)
              (format #t "(x - ~a)*(x - ~a)"
                      (/ (+ (- b)  (sqrt S)) (* 2 a))
                      (/ (- (- b)  (sqrt S)) (* 2 a))))))
      (poly-display poly)))

(define (poly2-display-par poly)
  (if (= (poly-order poly) 2)
      (let* ((a     (list-ref poly 2))
             (b/2a (/ (list-ref poly 1)
                      (* 2 a)))
             (c    (list-ref poly 0)))
        (format #t "~a * (x - ~a)**2 + ~a"
                a (- b/2a) (- c (expt b/2a 2))))
      (poly-display poly)))

(define (poly-print poly)       (poly-display poly) (newline))
(define (poly2-print-root poly) (poly-display-root poly) (newline))
(define (poly2-print-par poly)  (poly-display-par poly) (newline))

(define (make-poly-by-lsf n xlis ylis)  
  ;; least square fitting
  (polyNfit n xlis ylis))

;;;
;;;
;;;
(define (test-poly-display poly)
  (display "plot "
  (poly-display poly) 
  (display ", ")
  (poly2-display-root poly)
  (display ", ")
  (poly2-display-par poly)
  (newline)))

;;(test-poly-display '(-10 -2 1))

(define (test N NP)

  (define (fx x)  (sin (* 2 pi (- x 0.2))))
  (define fxstr   "sin(2*pi*(x-0.2))")

  (format #t "## Testing Order=~a, ~a points~%" N NP)

  (let* ((x (iota NP -1 (/ 2.01 NP)))
         (y (map fx x)))

    (let* ((fp  (polyNfit N x y))
           (ff  (poly->func fp))
           (ffx (map ff x))
           (sq  (map (lambda (x y) (* (- x y) (- x y))) y ffx))
           (ssq (apply + sq)))
      (format #t "## Sum of Squres= ~a~%" ssq)
      (format #t "plot [-1.1:1.1] ~a t 'Orig', ~a t 'Fit'~%" fxstr (poly->str fp))
      (format #t "pause -1 \"N=~a, NP=~a\"~%" N NP)
      )))

(test 1 1000)
(test 2 1000)
(test 3 1000)
(test 4 1000)
(test 5 1000)
(test 7 1000)
(test 8 1000)
(test 8 9)
(test 8 10)
(test 8 11)
(test 8 12)
(test 8 13)
(test 8 14)
(test 8 15)
(test 8 16)
(test 8 17)



