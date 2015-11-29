;;;
;;; From Kent Dybvig, THE SCHEME PROGRAMMING LANGUAGE SECOND EDITION,
;;; (http://www.scheme.com/tspl2d/)
;;;
(define-module ggc.lwp
  (use data.queue)
  (export lwp lwp-start lwp-pause lwp-next)
  )
(select-module ggc.lwp)

(define lwp-queue (make-queue))

(define (lwp thunk)
  (enqueue! lwp-queue (lambda () (thunk) (lwp-next))))

(define (lwp-start)
  (lwp-pause)
  (if (queue-empty? lwp-queue)
      #t
      (lwp-start)))

(define (lwp-next)
  (let ((next (dequeue! lwp-queue)))
    (next)))

(define (lwp-pause)
  (call/cc (lambda (k)
             (lwp (lambda () (k #f)))
             (lwp-next))))

(provide "ggc/lwp")
;; EOF

