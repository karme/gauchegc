;; parallel processing.
;;
(define-module ggc.lwp
    (export lwp lwp-init lwp-start lwp-pause lwp-exit))

(select-module ggc.lwp)

(define lwp-list '())
(define lwp-tail '())
(define lwp-max 0)

(define (lwp-init)
  (set! lwp-list '())
  (set! lwp-tail '())
  (set! lwp-max 0))

(define (addq e)
  (if (null? lwp-tail)
      (begin
	(set! lwp-tail (cons e '()))
	(set! lwp-list lwp-tail))
      (begin
	(set-cdr! lwp-tail (cons e '()))
	(set! lwp-tail (cdr lwp-tail))))
  (set! lwp-max (max lwp-max (length lwp-list))))

(define (headq)
  (let ((e (car lwp-list)))
    (set! lwp-list (cdr lwp-list))
    (if (null? lwp-list)
	(set! lwp-tail '()))
    e))

(define (lwp thunk)
  (addq thunk))

(define (start)
  (wait)
  (format (current-error-port) "Maximun processes=~a~%" lwp-max))

(define (wait)
  (pause)
  (if (not (null? lwp-list))
      (wait)))

(define (restart)
  (let ((next (headq)))
    (next)))

(define (pause)
  (call/cc (lambda (k)
	     (lwp (lambda () (k #f)))
	     (restart))))

(define lwp-start start)
(define lwp-pause pause)
(define lwp-exit  restart)
(provide "ggc/lwp")
;;;
; EOF
