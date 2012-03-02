(define-module parallel
  (use fork-process)
  (use util.stream)
  (use util.queue)
  ;;   (use control.job)
  ;;   (use control.thread-pool)
  (use srfi-1)
  (use serialization)
  (use gauche.selector)
  (use sxml.adaptor) ;; for assert
  (export bg-call
          bg-call-wait
          bg-call-selectable
          bg-call-ready?
          parallel-stream-map
          parallel-map
          stream->list-stream))

(select-module parallel)

(define (stream->list-stream count stream)
  (if (stream-null? stream)
    stream
    (stream-cons
     (stream->list (stream-take-safe stream count))
     (stream->list-stream count (stream-drop-safe stream count)))))

(define (bg-call proc . options)
  (let-keywords options ((handshake #t))
		(let1 child (fork-process (lambda()
					    (when handshake
						  (write '("hello"))
						  (flush))
                                            (serialize-result proc)
                                            (flush))
					  :input  "/dev/null"
					  :output :pipe)
                  (when handshake
                    (with-input-from-port (process-output child)
                      (lambda()
                        (let1 got (read)
                          (unless (equal? got '("hello"))
                            (error #`"handshake with ,child failed ,got"))))))
                  (list
                   (delay
                     (with-input-from-port (process-output child)
                       (lambda()
                         (unwind-protect
                          (deserialize-result)
                          (begin
                            ;; close input port to prevent dead-lock
                            (close-input-port (process-output child))
                            ;; todo:
                            ;; - maybe terminate process after some time?
                            ;; - really report error on non-zero exit?
                            ;;   (what if deserialize-result reported an error?)
                            (process-wait child #f #t))))))
                   child))))

;; wait for result
(define (bg-call-wait p)
  (force (car p)))

;; todo: bg-call-cancel?!

;; get selectable port
(define (bg-call-selectable p)
  (process-output (cadr p)))

;; result ready?
(define (bg-call-ready? p)
  (byte-ready? (process-output (cadr p))))

;; todo:
;; - might produce zombies
;;   we need some cleanup handler for the returned stream/queue!
;; - support multiple stream arguments, just like stream-map
(define (parallel-stream-map-bg-call proc stream . options)
  (let-keywords options ((limit 4)
                         (handshake #t)
                         (lookahead 8))
    (let* ((q (list))
           (outstanding (lambda() (length (filter (compose not cadr) q))))
           (selector (make <selector>))
           (launch (lambda(x)
                     (let1 b (list (bg-call (lambda() (proc x))
                                            :handshake handshake)
                                   #f)
                       (selector-add! selector
                                      (bg-call-selectable (car b))
                                      (lambda(p mode)
                                        (set! (cadr b) #t)
                                        (selector-delete! selector p
                                                          #f ;; todo
                                                          '(r)))
                                      '(r))
                       b)))
           (first-ready? (lambda() (cadr (car q))))
           (cleanup (lambda()
                      (while (not (null? q))
                        (guard (e
                                [else
                                 #t])
                               (unwind-protect
                                ;; todo: bg-call-cancel?
                                (bg-call-wait (car (car q)))
                                (pop! q))))))
           (get-first! (lambda()
                         (unwind-protect
                          (bg-call-wait (car (car q)))
                          (pop! q))))
           (refill (lambda(s)
                     (let1 tolaunch (min (- limit (outstanding))
                                         (- lookahead (length q)))
                       (set! q (append q (map launch
                                              (stream->list (stream-take-safe s tolaunch)))))
                       (stream-drop-safe s tolaunch)))))
      (guard (e [else
                 (cleanup)
                 (raise e)])
             (let loop ((s (refill stream)))
               (stream-delay
                (let loop2 ((s s))
                  (guard (e [else
                             (cleanup)
                             (raise e)])
                         (cond [(null? q)
                                stream-null]
                               [(not (first-ready?))
                                (selector-select selector)
                                (loop2 (refill s))]
                               [else
                                (stream-cons
                                 (guard (e [else
                                            (cleanup)
                                            (raise e)])
                                        (get-first!))
                                 (loop (refill s)))])))))))))

(define parallel-stream-map parallel-stream-map-bg-call)

;; todo: support multiple list arguments, just like map
(define (parallel-map proc l . args)
  (stream->list (apply parallel-stream-map (append (list proc (list->stream l))
                                                   args))))

(define (lookahead-stream stream lookahead)
  (let loop ((s (stream-drop-safe stream lookahead))
             (q (stream->list (stream-take-safe stream lookahead))))
    (stream-delay
     (if (null? q)
       stream-null
       (stream-cons
        (car q)
        (loop (stream-drop-safe s 1)
              (append (cdr q)
                      (stream->list (stream-take-safe s 1)))))))))

;; examples:
;; (define (timestamp-stream)
;;   (let* ((t (lambda()
;;               (receive (s us) (sys-gettimeofday)
;;                 (+ s (/. (quotient us 1000) 1000)))))
;;          (s (t)))
;;     (stream-tabulate -1 (lambda(x) (list x (- (t) s))))))

;; (let1 s
;;     ;;(timestamp-stream)
;;     (lookahead-stream (timestamp-stream) 2)
;;   (dotimes (i 10)
;;     (sys-sleep 1)
;;     #?=(stream-ref s i)))

;;(stream-for-each print (parallel-stream-map (cut apply + <...>) (stream->list-stream 10 (list->stream (circular-list 1 2 3)))))

;;(stream-for-each print (let1 s (timestamp-stream) (stream-append (stream-take-safe s 10) (stream-delay (begin (sys-sleep 1) (stream-drop-safe s 10))))))

;; (let ((l #?=(list-tabulate 2000 (lambda(x) (list-tabulate 3 (cut + <> 1000))))) (proc (lambda(l) (dotimes(i 1000) (apply *. l)) (apply + l)))) (equal? (time (map proc l)) (time (parallel-map proc l :limit 2))))
