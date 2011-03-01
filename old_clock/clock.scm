(define-module ggc.clock
  (export clock)
  (export runtime)
  (export measure-time)
  (export clocks-per-sec)
  (dynamic-load "ggcclock")
  )

(select-module ggc.clock)

(define (measure-time proc . args)
  (let ((s (clock))
	(cps (clocks-per-sec)))
    (let ((v (apply proc args)))
      (format (current-error-port)
	      "; Time ~s seconds~%" (/ (- (clock) s) cps))
      v)))

(define (runtime)
  (/ (clock) (clocks-per-sec)))

(provide "ggc/clock")