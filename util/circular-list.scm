(define-module ggc.util.circular-list
  (export-all)
)
(select-module ggc.util.circular-list)
;;;
;;;  CIRCULAR LIST UTILITIES
;;;
(define (circular-rotate cir)  (cdr cir))

(define-syntax circular-rotate!
  (syntax-rules ()
    ((_ loc)
     (set! loc (circular-rotate loc)))))

(define (circular-rotate-right cir)
  (let lp ((p cir))
    (cond ((eq? (cdr p) cir) p)
          (else (lp (cdr p))))))

(define-syntax circular-rotate-right!
  (syntax-rules ()
    ((_ loc)
     (set! loc (circular-rotate-right loc)))))

(define (circular-push! cir val)
  (let ((v (car cir)))
    (set-car! cir val)
    (set-cdr! cir (cons v (cdr cir)))))

(define-syntax circular-pop!
  (syntax-rules ()
    ((_ loc)
     (let ((v  (car loc))
           (c1 (circular-rotate-right loc)))
       (set-cdr! c1 (cdr loc))
       (set! loc (cdr c1))
       v))))

(define-syntax circular-append!
  (syntax-rules ()
    ((_ loc val)
     (begin
       (circular-push! loc val)
       (set! loc (circular-rotate loc))))))

(define (circular-for-each proc cir)
  (let lp ((p cir))
    (if (not (eq? (cdr p) cir))
        (begin
          (proc (car p))
          (lp (cdr p))))))
      
(define (circular-find cir pred)
  (let lp ((p cir))
    (cond ((pred (car p)) p)
          ((eq? (cdr p) cir) #f)
          (else (lp (cdr p))))))

(define-syntax circular-find!
  (syntax-rules ()
    ((_ loc val)
     (let ((p (circular-find loc pred)))
       (if p
           (begin
             (set! loc p)
             #t)
           #f)))))

(define (circular-find-and-bring-it-top! cir pred)
  (if (pred (car cir)) 
      (car cir)
      (let lp ((p cir)
               (q (cdr cir)))
        (cond ((pred (car q))
               (set-cdr! p (cdr q))
               (let ((r (car cir))
                     (s (cdr cir)))
                 (set-car! cir (car q))
                 (set-cdr! cir (cons r s))
                 (car q)))
              ((eq? q cir) #f)
              (else (lp (cdr p) (cdr q)))))))

(define (circular->proper cir)
  (let lp ((p cir)
           (q '()))
    (cond ((eq? (cdr p) cir) 
           (reverse (cons (car p) q)))
          (else
           (lp (cdr p) (cons (car p) q))))))

(provide "ggc/util/circular-list")

;;; EOF
