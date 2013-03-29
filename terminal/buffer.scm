#| -*- mode: scheme; coding: utf-8; -*- |#

;; a simple text buffer
;;
;; todo:
;;
;; - is using u16vector really a good idea? (utf-16?)
;;   for now this should be ok
;;
;; - don't use space for erased positions
;;
;; - add color/underline/... support
;;
;; - rename to text-buffer?
;;
;; - double width chars?
;;
;; - suitable for proportional font representation?
;;
;; - don't allow to set x,y,width,height,text slots from outside

(define-module buffer
  (use srfi-1)
  (use gauche.uvector)
  (export 
   make-buffer
   buffer-clear-row
   buffer-clear
   buffer-move-x
   buffer-move
   buffer-check
   buffer-set-char!))

(select-module buffer)

(define-macro (debug-assert e)                                                   
  `(if (not ,e)                                                            
     (error "Assertion failed: " ,(x->string e))))                       

;; todo
(define (double-width-char c)
  #f)

(define *empty-char*
  (char->integer #\space))

(define (buffer-append-empty-line buffer)
  (define (empty-line)
    (make-u16vector (ref buffer 'width) *empty-char*))
  
  (set! (ref buffer 'text) 
	(append (ref buffer 'text) (list (empty-line)))))

(define-class <buffer> ()
  ((x :init-value 0)
   (y :init-value 0)
   text
   width
   height
   ))

(define-method buffer-clear-row ((buffer <buffer>) . args)
  (define (buffer-row)
    (ref (slot-ref buffer 'text) (slot-ref buffer 'y)))

  (let-optionals* args ((start 0)
			(end (ref buffer 'width)))
    (u16vector-fill! (buffer-row) *empty-char* start end)))

(define-method buffer-clear ((buffer <buffer>))
  (slot-set! buffer 'text '())
  (slot-set! buffer 'x 0)
  (slot-set! buffer 'y 0)
  (dotimes (i (ref buffer 'height))
    (buffer-append-empty-line buffer))
  (buffer-check buffer))

;; move by dx columns
;; note: no wrap-around and no clip!
(define-method buffer-move-x ((buffer <buffer>) dx)
  (inc! (ref buffer 'x) dx)
  (debug-assert (and (>= (ref buffer 'x) 0)
                     (< (ref buffer 'x) (ref buffer 'width)))))

;; relative cursor movement
;; note: with wrap-around
(define-method buffer-move ((buffer <buffer>) dx dy)

  (define (buffer-move-y-up dy)
    (buffer-check buffer)
    (debug-assert (>= dy 0))
    (when (> dy 0)
      (when (<= (ref buffer 'y) 0)
	(error "already at top"))
      (dec! (ref buffer 'y) 1)
      (buffer-check buffer)
      (buffer-move-y-up (- dy 1))))

  (define (buffer-move-y-down dy)
    (buffer-check buffer)
    (when (> dy 0)
      (inc! (ref buffer 'y) 1)
      (when (>= (ref buffer 'y) (size-of (ref buffer 'text)))
	(buffer-append-empty-line buffer)
        (when (> (size-of (ref buffer 'text)) 500)
          (set! (ref buffer 'text) (cdr (ref buffer 'text)))
          (dec! (ref buffer 'y) 1)))
      (buffer-check buffer)
      (buffer-move-y-down (- dy 1))))

  (define (buffer-move-y dy)
    (if (> dy 0)
      (buffer-move-y-down dy)
      (buffer-move-y-up (- dy))))

  (define (wrap-x xy max-x)
    (cons (modulo (car xy) max-x) (+ (cdr xy) (quotient (car xy) max-x))))

  (let1 p (wrap-x (cons (+ (ref buffer 'x) dx) (+ (ref buffer 'y) dy))
                  (ref buffer 'width))
    (buffer-check buffer)	
    (buffer-move-x buffer (- (car p) (ref buffer 'x)))
    (buffer-move-y (- (cdr p) (ref buffer 'y)))))

(define-method buffer-check ((buffer <buffer>))
  (debug-assert (< (ref buffer 'y) (size-of (ref buffer 'text)))))

;; set character at the current buffer position
(define-method buffer-set-char! ((buffer <buffer>) c)
  (when (double-width-char c)
    (error "not implemented"))
  (buffer-check buffer)
  (u16vector-set! (ref (ref buffer 'text) (slot-ref buffer 'y)) (slot-ref buffer 'x) c))

(define (make-buffer width height)
  (let1 buffer (make <buffer>)
    ;; todo hack: +1 for "auto-margin"
    (slot-set! buffer 'width (+ width 1))
    (slot-set! buffer 'height height)
    (buffer-clear buffer)
    buffer))
