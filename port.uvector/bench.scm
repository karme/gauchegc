(use gauche.time)
(use gauche.uvector)
(use ggc.port.uvector)

(define buff #f)  ;; to be set! u8vector

(define ta (make <real-time-counter>))

;;;
;;; read m bytes
;;; 
(define (tst0 m)
  (if (= m 0) 
      m
      (begin
        (read-byte)
        (tst0 (- m 1)))))

;;;
;;; procedural port
;;;
(define (tst1 m)
  (with-input-from-u8vector/pp
   buff 0 m 
   (lambda () (tst0 m))))

;;;
;;; string port
;;;
(define (tst2 m)
  (with-input-from-u8vector/sp
   buff 0 m 
   (lambda () (tst0 m))))

;;;
;;; mesure time
;;;
(define (tst proc n m)
  (time-counter-reset! ta)
  (with-time-counter ta 
                     (do ((i 1 (+ i 1)))
                         ((> i n) 0)
                       (proc m)))
  (format #t "~a ~a ~a~%" n m (time-counter-value ta)))

(define (main args)
  (if (= (length args) 4)
      (let ((n (string->number (list-ref args 2)))
            (m (string->number (list-ref args 3))))
        (case (string->number (cadr args))
          ((0) 
           (tst tst0 n m))
          ((1) 
           (set! buff (make-u8vector m 0))
           (tst tst1 n m))
          ((2) 
           (set! buff (make-u8vector m 0))
           (tst tst2 n m))
          (else (error "unknown funtion"))))
      (error "read-byte f n m")))
