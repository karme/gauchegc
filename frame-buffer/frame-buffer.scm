(define-module ggc.skimu.frame-buffer
  (use gauche.uvector)
  (use gauche.process)
  (export make-frame-buffer
	  read-ppm
	  write-ppm
	  load-frame-buffer-from-ppm-file
	  load-frame-buffer-from-png-file
	  load-frame-buffer-from-jpg-file
	  load-frame-buffer-from-gif-file
	  save-frame-buffer-as-ppm-file
	  save-frame-buffer-as-png-file
	  save-frame-buffer-as-jpg-file
	  fb-draw-line!
	  fb-draw-box!
	  fb-fill!
	  fb-clear!
	  fb-draw-points!
	  fb-view
	  )
  )

(select-module ggc.skimu.frame-buffer)

(define (make-frame-buffer w h)
  (let ((img (make-u8vector (* w h 3) 255)))
    (define (check-region x y)
      (and (< x w) (>= x 0) 
	   (< y h) (>= y 0)))
    (define (frame-buffer-set! x y r g b)
      (and (check-region x y)
	   (let ((offset (+ (* 3 y w) (* 3 x))))
	     (u8vector-set! img offset r)
	     (u8vector-set! img (+ offset 1) g)
	     (u8vector-set! img (+ offset 2) b))))
    (define (frame-buffer-ref x y)
      (and (check-region x y)
	   (let ((offset (+ (* 3 y w) (* 3 x))))
	     (values (u8vector-ref img offset)            ; r
		     (u8vector-ref img (+ offset 1))      ; g
		     (u8vector-ref img (+ offset 2))))))  ; b
    (lambda (m . args)
      (cond ((eq? m 'set!) (apply frame-buffer-set! args))
	    ((eq? m 'ref)  (apply frame-buffer-ref args))
	    ((eq? m 'img) img)
	    ((eq? m 'width) w)
	    ((eq? m 'height) h)))))

(define (read-ppm)
  ;; XXX - todo  # comment.
  (and (eq? 'P6 (read))
       (begin
	 (let ((w (read))
	       (h (read))
	       (d (read)))
	   (and (<= d 255)
		(read-byte);; skip single byte white space
		(let ((fb  (make-frame-buffer w h))
                      (p   (current-input-port)))
                  (set! (port-buffering p) :full) ; make sure full-buffering
                  (read-block! (fb 'img) p 0 (* w h 3))
                  fb))))))

(define (write-ppm x y v)
  (display "P6")(newline)
  (display x)(display " ")(display y)(newline)
  (display 255)(newline)
  (write-block v)
  (flush))

;;;
;;; Benchmark 0.6.4 write-block read-block!
;;;
#|
wugga% time ./makegauss.scm 600 600 out.ppm (write-block)
OLD
200x200 real 0m 1.893s  user    0m1.600s  sys     0m0.130s
400x400 real 0m 6.043s  user    0m5.490s  sys     0m0.180s
600x600 real 0m13.429s  user    0m11.970s sys     0m0.190s

NEW
200x200 real 0m 1.649s  user    0m1.350s  sys     0m0.130s
400x400 real 0m 5.040s  user    0m4.530s  sys     0m0.110s
600x600 real 0m10.883s  user    0m9.840s  sys     0m0.190s

wugga% time ./gradimage.scm out600x600.jpg fo.jpg
OLD real    1m8.910s user    1m 2.300s sys     0m0.690s
NEW real    1m3.611s user    0m58.380s sys     0m0.630s
|#


(define (load-frame-buffer-from-ppm-file fname)
  (with-input-from-file fname
    read-ppm))

(define (load-frame-buffer-from-png-file fname)
  (with-input-from-process (string-append "pngtopnm 2> /dev/null " fname)
                           read-ppm))

(define (load-frame-buffer-from-gif-file fname)
  (with-input-from-process (string-append "giftopnm 2> /dev/null " fname)
                           read-ppm))

(define (load-frame-buffer-from-jpg-file fname)
  (with-input-from-process (string-append "djpeg 2> /dev/null " fname)
                           read-ppm))

(define (save-frame-buffer-as-ppm-file fb fname)
  (with-output-to-file fname
    (lambda ()
      (write-ppm (fb 'width) (fb 'height) (fb 'img)))))

(define (save-frame-buffer-as-png-file fb fname)
  (with-output-to-process (string-append "pnmtopng 2> /dev/null > " fname)
    (lambda ()
      (write-ppm (fb 'width) (fb 'height) (fb 'img)))))

(define (save-frame-buffer-as-jpg-file fb fname)
  (with-output-to-process (string-append "cjpeg 2> /dev/null > " fname)
    (lambda ()
      (write-ppm (fb 'width) (fb 'height) (fb 'img)))))

(define (fb-draw-line! fb x1 y1 x2 y2 red green blue . rest)
  (let ((d (round->exact (sqrt (+ (expt (- x2 x1) 2)
                                  (expt (- y2 y1) 2))))))
    (if (= d 0)
        (fb 'set! x1 y1 red green blue)
        (do ((t1 0 (+ t1 1)))
            ((> t1 d) #t)
          (let ((t2 (- d t1)))
            (let ((x  (round->exact (/ (+ (* t1 x1) (* t2 x2)) d)))
                  (y  (round->exact (/ (+ (* t1 y1) (* t2 y2)) d)))) 
              (fb 'set! x y red green blue)))))))

(define (fb-draw-break-line! fb x1 y1 x2 y2 red green blue . rest)
  (define a 1)
  (define b 1)
  (define draw?
    (let ((state #t))
      (lambda (t)
	(if state 
	    (begin 
	      (if (= (modulo t a) 0) 
		  (set! state #f))
	      #t)
	    (begin
	      (if (= (modulo t b) 0)
		  (set! state #t))
	      state)))))
      
  (if (not (null? rest))
      (begin
	(set! a (car rest))
	(set! b (cadr rest))))

  (let ((d (round->exact (sqrt (+ (expt (- x2 x1) 2)
                                  (expt (- y2 y1) 2))))))
    (if (= d 0)
        (fb 'set! x1 y1 red green blue)
        (do ((t1 0 (+ t1 1)))
            ((> t1 d) #t)
          (let ((t2 (- d t1)))
            (if (draw? t1)
                (let ((x  (round->exact (/ (+ (* t1 x1) (* t2 x2)) d)))
                      (y  (round->exact (/ (+ (* t1 y1) (* t2 y2)) d)))) 
                  (fb 'set! x y red green blue))))))))

(define (fb-draw-box! fb x1 y1 x2 y2 r g b)
  (fb-draw-line! fb x1 y1 x2 y1 r g b)
  (fb-draw-line! fb x2 y1 x2 y2 r g b)
  (fb-draw-line! fb x2 y2 x1 y2 r g b)
  (fb-draw-line! fb x1 y2 x1 y1 r g b))

(define (fb-fill! fb x1 y1 x2 y2 r g b)
  (if (> x1 x2) (fb-fill! fb x2 y1 x1 y2 r g b))
  (if (> y1 y2) (fb-fill! fb x1 y2 x2 y1 r g b))
  (do ((y y1 (+ y 1)))
      ((= y y2) #t)
    (do ((x x1 (+ x 1)))
	((= x x2) #t)
      (fb 'set! x y r g b))))

(define (fb-clear! fb)
  (fb-fill! fb 0 0
	   (- (fb 'width) 1)
	   (- (fb 'height) 1)
	   255 255 255))

(define (fb-draw-points! fb points)
  (define (draw-points points-length points-ref)
    (let ((N (points-length points)))
      (do ((i 0 (+ i 1)))
	  ((= i N) #t)
	(let ((p (points-ref points i)))
	  (fb 'set! (car p) (cdr p) 0 0 0)))))
  (cond ((pair? points)
	 (for-each (lambda (p) (fb 'set! (car p) (cdr p) 0 0 0)) points))
	((vector? points)
	 (draw-points vector-length vector-ref))
	(else
	 (error "fb-draw-points!: bad points"))))

(define fb-view-command 
  (if (sys-access "/usr/bin/open" X_OK)
      "open ~a"       ; Mac OS X
      "xv ~a &"))     ; Others (X11)

(define (fb-view fb)
  (let ((pngname (string-append "fbv"
				(number->string 
				 (sys-random))
				".png")))
    (dynamic-wind 
	(lambda () #t)
	(lambda ()
	  (save-frame-buffer-as-png-file fb pngname)
	  (sys-system (format #f fb-view-command pngname))
	  (sys-sleep 2))
	(lambda ()
	  (and (file-exists? pngname)
	       (sys-remove pngname))))))

(provide "ggc/skimu/frame-buffer")

;;; Example: SICP paint language
;
; (use frame-buffer)
;
; (define the-fb '())
; (define (draw-line p1 p2)
;   (let* ((x1 (xcor-vect p1))
; 	 (y1 (ycor-vect p1))
; 	 (x2 (xcor-vect p2))
; 	 (y2 (ycor-vect p2)))
;     (fb-draw-line! the-fb
; 		  (round->exact x1)
; 		  (round->exact y1)
; 		  (round->exact x2)
; 		  (round->exact y2)
; 		  0 0 0)))
;
; (define (apply-painter-to-png-file painter width height filename)
;   (let ((fb     (make-frame-buffer width height))
;   	(frame  (make-frame (make-vect 0.0 height)
; 			    (make-vect width 0.0)
; 			    (make-vect 0.0 (- height)))))
;     (set! the-fb fb)
;     (painter frame)
;     (save-frame-buffer-as-png-file the-fb filename)))
;
; (define (x-mark frame)
;   (let ((tl (make-vect 0 1))
; 	(tr (make-vect 1 1))
; 	(bl (make-vect 0 0))
; 	(br (make-vect 1 0)))
;     ((segments->painter
;       (list (make-segment tl br)
; 	    (make-segment tr bl))) frame)))
;
; (define csx8 (corner-split x-mark 8))
;
; (apply-painter-to-png-file csx 480 480 "csx8.png")
;; EOF

