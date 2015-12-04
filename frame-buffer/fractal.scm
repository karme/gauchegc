(use gauche.time)
(use srfi-1)
(use ggc.skimu.frame-buffer)

(define (draw-line fb p1 p2)
  (let* ((x1 (xcor-vect p1))
         (y1 (ycor-vect p1))
	 (x2 (xcor-vect p2))
         (y2 (ycor-vect p2)))
    (fb-draw-line! fb
                   (round->exact x1)
                   (round->exact y1)
                   (round->exact x2)
                   (round->exact y2)
                   0 0 0)
    ))

(define (draw-painter painter width height filename)
  (let* ((fb     (make-frame-buffer width height))
         (frame  (make-frame fb
                             (make-vect 0.0 height)
                             (make-vect width 0.0)
                             (make-vect 0.0 (- height)))))
    (painter frame)
    (save-frame-buffer-as-png-file fb filename)
    ))

(define (run n)
  (let ((d (dragon simple-segment n))
        (k (koch   simple-segment n))
        (l (levy   simple-segment n)))
    (print n)
    (draw-painter (shift (scale d 0.6) (make-vect 0.25 0.60)) 400 400 (format #f "dragon~2,'0d.png" n))
    (draw-painter (shift k             (make-vect 0.00 0.50)) 400 400 (format #f "koch~2,'0d.png" n))
    (draw-painter (shift (scale l 0.5) (make-vect 0.25 0.65)) 400 400 (format #f "levy~2,'0d.png" n)))
    (sys-system "convert -loop 15 dragon*.png dragon.gif")
    (sys-system "convert -loop 15 koch*.png   koch.gif")
    (sys-system "convert -loop 15 levy*.png   levy.gif")
  )

(for-each run (iota 15))

(exit 0)
;;;
;;;  Animation version
;;;
(define base   "aa")
(define count  0)

(define (draw-line fb p1 p2)
  (let* ((x1 (xcor-vect p1))
         (y1 (ycor-vect p1))
	 (x2 (xcor-vect p2))
         (y2 (ycor-vect p2)))
    (fb-draw-line! fb
                   (round->exact x1)
                   (round->exact y1)
                   (round->exact x2)
                   (round->exact y2)
                   0 0 0)
    (save-frame-buffer-as-png-file fb (format #f "~a/~5,'0d.png" base count))
    (inc! count)
    ))

(define (draw-painter painter width height filename)
  (let* ((fb     (make-frame-buffer width height))
         (frame  (make-frame fb
                             (make-vect 0.0 height)
                             (make-vect width 0.0)
                             (make-vect 0.0 (- height)))))
    (set! count   0)
    (set! base filename)
    (sys-system #"mkdir -p ~|base|")
    (painter frame)
    ))

(define dragon (dragon point 8))
(define koch   (koch   simple-segment 9))
(define levy   (levy   point 8))

(time (draw-painter (shift (scale dragon 0.5) (make-vect 0.25 0.50)) 400 400 "dragon"))
(time (draw-painter (shift koch               (make-vect 0.00 0.50)) 400 400 "koch"))
(time (draw-painter (shift (scale levy   0.6) (make-vect 0.12 0.45)) 400 400 "levy"))
