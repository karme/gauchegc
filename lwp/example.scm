(use ggc.lwp)

(print "Example1:")

(define (disp c)
  (lwp-pause)
  (display c))

(lwp (lambda () (for-each disp (string->list "147"))) )
(lwp (lambda () (for-each disp (string->list "258"))) )
(lwp (lambda () (for-each disp (string->list "369"))) )

(lwp-start)
(newline)


(print "Example2:")

(define (job1)
  (let loop ((lis '(1 2 3 4 5 6 7 8 9)))
    (lwp-pause)
    (cond ((null? lis) #t)
          (else
           (print #"(jobs1):~(car lis)")
           (loop (cdr lis))))))

(define (job2)
  (let loop ((lis '(1 2 3 4 5 6 7 8 9))
             (acc  0))
    (lwp-pause)
    (cond ((null? lis) (print #"(jobs2)=~|acc|"))
          (else
           (print #"(jobs2):~|acc|")
           (loop (cdr lis) (+ acc (car lis)))))))

(define (job3)
  (let loop ((lis '(1 2 3 4 5 6 7 8 9))
             (acc  1))
    (lwp-pause)
    (cond ((null? lis) (print #"(jobs3)=~|acc|"))
          (else
           (print #"(jobs3):~|acc|")
           (loop (cdr lis) (* acc (car lis)))))))

(lwp job1)
(lwp job2)
(lwp job3)
(lwp-start)

;;; EOF
