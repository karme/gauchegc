;; Gauche Garbage Collection, Clock procedures.
;; written by skimu@mac.com. see ggc/COPYING for detail.
;;
(define-module ggc.clock
  (use gauche.version)
  (export *clocks-per-sec*
	  clock
          runtime
	  measure-time
	  time
	  )
)

(select-module ggc.clock)

(define *clocks-per-sec*
  (list-ref (sys-times) 4))

(define (clock)
  (list-ref (sys-times) 0)) ; tms_utime

(define (runtime)
  (/ (clock) *clocks-per-sec*))

(define (measure-time proc . args)
  (let ((stms (sys-times)))
    (let ((v   (apply proc args)))
      (let ((etms (sys-times)))
	(define (diff-in-sec n)
	  (/ (- (list-ref etms n) (list-ref stms n)) *clocks-per-sec*))
	(format (current-error-port)
		"; Time ~as user   ~as system~%"
		(diff-in-sec 0)     ; 0:utime 1:stime
		(diff-in-sec 1))) 
      v)))

;;;
;;; gauche.time appeared in 0.6.4 obsoletes ggc.clock's time
;;;

#|
(define (version->integer str)
  (let ((v (map string->number (string-split str #\.))))
    (if (= (length v) 3)
        (+ (* (car v) 1000000)
           (* (cadr v)   1000)
           (caddr  v))
        (errorf "version->integer: invalid version string ~a" str))))
|#

(define-macro (time exp)
  (let ((sta (gensym))
        (ret (gensym)))
    `(let ((,sta (runtime))
           (,ret  ,exp))
       (if (version>=? (gauche-version) "0.6.4")
           (display "Warning: time macro is obsoleted by gauche.time"
                    (current-error-port)))
       (format (current-error-port)
               "; ~a seconds for ~a~%" (- (runtime) ,sta) ',exp)
       ,ret)))

(provide "ggc/clock")
;; EOF