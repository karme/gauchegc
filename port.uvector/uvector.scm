(define-module ggc.port.uvector
  (use gauche.uvector)
  (export-all)
)

(select-module ggc.port.uvector)

;;;
;;; This is  much faster than procedural-port version.
;;;
(define (with-input-from-u8vector/sp vec start end thunk)
  (with-input-from-string (u8vector->string 
                           (uvector-alias <u8vector>
                                          vec
                                          start
                                          end))
    thunk))

(define with-input-from-u8vector with-input-from-u8vector/sp)

;;;
;;;
;;;
(define *eof* (with-input-from-string "" read-char))

(define (open-input-u8vector vec . args)

  (define (make-filler vec pointer end)
    (let ((buf (make-byte-string 1)))
      (lambda (n)
        (if (>= pointer end) 
            *eof*
            (let ((b (u8vector-ref vec pointer)))
              (string-byte-set! buf 0 b)
              (inc! pointer)
              buf)))))

  (let-optionals* args ((start 0)
                        (end (u8vecotr-length vec)))
    (open-input-buffered-port (make-filler vec start end) 2)))

;;;
;;;
;;;
(define (call-with-input-u8vector vec start end proc)
  (let ((p #f))
    (dynamic-wind
        (lambda () (set! p (open-input-u8vector vec start end)))
        (lambda () (proc p))
        (lambda () (close-input-port p)))))

;;;
;;;
;;;
(define (with-input-from-u8vector/pp vec start end thunk)
  (call-with-input-u8vector 
   vec start end (lambda (p) (with-input-from-port p thunk))))


(provide "ggc/port/uvector")

#|
(define cc (u8vector 65 66 67 68))
cc ;; -> u8(65 66 67 68)
(define ss (u8vector->string cc))
ss ;; -> "ABCD"
(u8vector-set! cc 2 69) ;; -> u8(65 66 69 68)
ss ;; -> "ABCD" (;_;)
(uvector-alias <string> cc) ;; -> error

;;; test
(call-with-input-u8vector cc 0 3
                          (lambda (p) 
                            (port-for-each (lambda (x)
                                             (print x))
                                           (lambda ()
                                             (read-byte p)))))
(with-input-from-u8vector cc 0 3
                          (lambda () (port-for-each print read-byte)))

(define filler (make-filler aa  1 3))
(define filler2 (make-filler aa  0 5))
(filler 1)
(filler2 1)

|#

