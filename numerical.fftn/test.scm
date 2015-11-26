;;
;; 
;;
(use srfi-1)
(use ggc.numerical.fftn)

(define (comp lis1 lis2)
  (let ((dif (map (lambda (x y) 
                    (abs (- x y)))
                  lis1 lis2)))
    (apply + dif)))

(define (test-fft N)
  (let* ((in  (iota N))
         (o1  (fftn-fft  in))
         (out (fftn-rfft o1)))
    (print (comp in out))))

(define (main args)
  (if (= (length args) 2)
      (test-fft (string->number (cadr args)))
      (error "Usage: test N"))
  0)
