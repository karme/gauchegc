;; notes:
;; - ideally read and write/ss would be good enough
;; - kahua has some serialization/persistance
(define-module serialization
  (use util.isomorph)
  (export serialize
          serializable?
          deserialize
          <serializable-error>
          serialize-result
          deserialize-result))
(select-module serialization)

;; note: write-object is only used for user defined obects!
;; => we can not provide write-object for <error>
(define-class <serializable-error> (<error>) ())
(define-method write-object ((e <serializable-error>) out)
  (format out "#,(serializable-error ~s)" (ref e 'message)))
(define-method object-equal? ((e1 <serializable-error>) (e2 <serializable-error>))
  (equal? (ref e1 'message) (ref e2 'message)))
(define-reader-ctor 'serializable-error (lambda(x)
                                          (make <serializable-error> :message x)))

;; (define-method write-object ((d <dictionary>) out)
;;   (format out "#,(dictionary ~a ~a)" (ref (class-of d) 'name) (dict-map d cons)))

(define (write/ss-to-string x)
  (with-output-to-string (lambda() (write/ss x))))

;; todo: simpler way to detect wether object is serializable?
(define (serializable? x)
  (guard (e [else
             #f])
         (isomorphic? (read-from-string (write/ss-to-string x))
                      x)))

(define (serialize . args)
  ;; todo:
  ;; - do we really want to allow cyclic structures?
  (when (not (serializable? (car args)))
    (error "object not serializable"))
  (apply write/ss args))

(define (deserialize . args)
  (apply read args))

(define (serialize-result thunk . args)
  (guard (e [(<error> e)
             ;; todo:
             ;; - loosing too much information, here
             ;; - at least provide a stack trace
             ;; - how preserve error subclass?
             (apply serialize
                    (cons (make <serializable-error> :message (ref e 'message))
                          args))]
            [else
             (apply serialize
                    (cons (make <serializable-error> :message "unkown serialization error")
                          args))])
         (apply serialize
                (cons
                 (guard (e [(<error> e)
                            ;; todo:
                            ;; - loosing too much information, here
                            ;; - at least provide a stack trace
                            ;; - how preserve error subclass?
                            (make <serializable-error> :message (ref e 'message))]
                           [else
                            (make <serializable-error> :message "unknown error")])
                        (receive l (thunk) l))
                 args))))

(define (deserialize-result . args)
  (let1 r (apply deserialize args)
    (cond [(<serializable-error> r)
           (error (ref r 'message))]
          [(list? r)
           (apply values r)]
          [else
           (error "protocol error")])))
