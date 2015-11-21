(define-module ggc.img.eps2x
  (use srfi-13)
  (use gauche.process)
  (export eps2x)
)
(select-module ggc.img.eps2x)

;;;
;;;
;;;
(define (postscript? port)
  (rxmatch #/^%!/ (read-line port)))

(define (get-bounding-box port)
  (let loop ((n 20) (str (read-line port)))
    (cond ((<= n 0) #f)
          ((eof-object? str) #f)
          ((rxmatch #/^%%EndComments/ str) #f)
          ((rxmatch #/^%%BoundingBox:/ str)
           (map string->number (string-tokenize str #[\d+-])))
          (else
           (loop (- n 1) (read-line port))))))
;;;
;;;
;;;
(define (eps2x from to dev res)
  (let ((bbox (call-with-input-file from
                (lambda (port)
                  (if (postscript? port)
                      (get-bounding-box port)
                      (errorf "~a is not a postscript file~%" from))))))
    (if bbox
	(receive (x1 y1 x2 y2) (apply values bbox)
	  (define (pt->dot pt)
	    (inexact->exact (ceiling (* (/ pt 72) res))))
          ;;(write bbox)
          ;;(newline)
          (receive (gs pid) (open-output-process-port 
                             (format #f
    "gs -q -sDEVICE=~a  -g~ax~a -r~a -dNOPAUSE -sOutputFile=~a - ~a"
                             dev
			     (pt->dot (- x2 x1))
			     (pt->dot (- y2 y1))
			     res 
                             to from))
	    (format gs "~a ~a translate~%" (- x1)(- y1))
            (close-output-port gs)
            (process-wait pid)))
	(errorf "Could not find BoundingBox~%"))))

(provide "ggc/img/eps2x")

; EOF
