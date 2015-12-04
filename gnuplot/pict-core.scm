;;; SICP picture language

;;  implementation of vector

(define xcor-vect car)
(define ycor-vect cdr)
(define make-vect cons)

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

;; implementation of segment

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line (window-frame frame)
                  ((frame-coord-map frame) (start-segment segment))
                  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (p-dot frame)
  (draw-dot (window-frame frame)
              ((frame-coord-map frame) (make-vect 0.0 0.0))))

;; implementation of frame

(define (make-frame w origin edge1 edge2)
  (list w origin edge1 edge2))

(define (window-frame frame)
  (car frame))
(define (origin-frame frame)
  (cadr frame))
(define (edge1-frame frame)
  (caddr frame))
(define (edge2-frame frame)
  (cadddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; painter operations

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((w (window-frame frame))
          (m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame w 
                     new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (identity x) x)

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (scale painter r)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect  r  0.0)
                     (make-vect 0.0  r )))
(define (scale2 painter x y)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect  x  0.0)
                     (make-vect 0.0  y )))

(define (shift painter s)
  (transform-painter painter
                     s
                     (add-vect (make-vect 1.0 0.0) s)
                     (add-vect (make-vect 0.0 1.0) s)))

(define (rotate painter t)
  (let ((s (sin t))
        (c (cos t)))
    (transform-painter painter
                       (make-vect  0.0 0.0)
                       (make-vect    c   s)
                       (make-vect (- s) c))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-above
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (cross-limit painter n)
  (if (= n 0)
      painter
      (let ((top    (beside painter (cross-limit painter (- n 1))))
	    (bottom (beside (cross-limit painter (- n 1)) painter)))
	(below bottom top))))

(define (compose-painter . painters)
  (lambda (frame)
    (for-each (lambda (painter) (painter frame)) painters)))


;; fractal transformation.

(define ir2  (/ 1 (sqrt 2)))
(define ir3  (/ 1 (sqrt 3)))
(define pi/6 (/ pi 6))

(define (dragon painter n)
  (if (= n 0)
      painter
      (let ((f1 (scale (rotate painter (- pi/4)) ir2))
            (f2 (shift (scale (rotate (scale2 painter -1 -1) pi/4) ir2)
                       (make-vect 1.0 0.0))))
        (dragon (compose-painter f1 f2) (- n 1)))))

(define (levy painter n)
  (if (= n 0)
      painter
      (let ((f1 (scale (rotate painter (- pi/4)) ir2))
            (f2 (shift (scale (rotate (shift painter 
                                             (make-vect -1.0 0.0))
                                      pi/4)
                              ir2)
                       (make-vect 1.0 0.0))))
        (levy (compose-painter f1 f2) (- n 1)))))

(define (koch painter n)
  (if (= n 0)
      painter
      (let ((f1 (scale (rotate (scale2 painter 1 -1) pi/6) ir3))
            (f2 (shift (scale (rotate (shift (scale2 painter 1 -1)
                                             (make-vect -1.0 0.0))
                                      (- pi/6))
                              ir3)
                       (make-vect 1.0 0.0))))
        (koch (compose-painter f1 f2) (- n 1)))))


;; painter

(define (x-mark frame)
  (let ((tl (make-vect 0 1))
	(tr (make-vect 1 1))
	(bl (make-vect 0 0))
	(br (make-vect 1 0)))
    ((segments->painter
      (list (make-segment tl br)
	    (make-segment tr bl))) frame)))

(define (wave frame)
  (let ((p01 (make-vect 0.40 1.00))
	(p02 (make-vect 0.60 1.00))
	(p03 (make-vect 0.00 0.80))
	(p04 (make-vect 0.35 0.80))
	(p05 (make-vect 0.65 0.80))
	(p06 (make-vect 0.00 0.60))
	(p07 (make-vect 0.30 0.60))
	(p08 (make-vect 0.40 0.60))
	(p09 (make-vect 0.60 0.60))
	(p10 (make-vect 0.70 0.60))
	(p11 (make-vect 0.20 0.55))
	(p12 (make-vect 0.30 0.55))
	(p13 (make-vect 0.35 0.50))
	(p14 (make-vect 0.65 0.50))
	(p15 (make-vect 0.20 0.45))
	(p16 (make-vect 1.00 0.40))
	(p17 (make-vect 0.50 0.20))
	(p18 (make-vect 1.00 0.20))
	(p19 (make-vect 0.25 0.00))
	(p20 (make-vect 0.40 0.00))
	(p21 (make-vect 0.60 0.00))
	(p22 (make-vect 0.75 0.00)))
    ((segments->painter
      (list (make-segment p01 p04)
	    (make-segment p04 p08)
	    (make-segment p08 p07)
	    (make-segment p07 p11)
	    (make-segment p11 p03)
	    (make-segment p06 p15)
	    (make-segment p15 p12)
	    (make-segment p12 p13)
	    (make-segment p13 p19)
	    (make-segment p20 p17)
	    (make-segment p17 p21)
	    (make-segment p22 p14)
	    (make-segment p14 p18)
	    (make-segment p16 p10)
	    (make-segment p10 p09)
	    (make-segment p09 p05)
	    (make-segment p05 p02))) frame)))

(define (border frame)
  (let ((tl (make-vect 0 1))
	(tr (make-vect 1 1))
	(bl (make-vect 0 0))
	(br (make-vect 1 0)))
    ((segments->painter
      (list (make-segment tl tr)
	    (make-segment tr br)
	    (make-segment br bl)
	    (make-segment bl tl))) frame)))

(define (point frame)
  (let ((tl (make-vect 0.25 0.75))
	(tr (make-vect 0.75 0.75))
	(bl (make-vect 0.25 0.25))
	(br (make-vect 0.75 0.25)))
    ((segments->painter
      (list (make-segment tl tr)
	    (make-segment tr br)
	    (make-segment br bl)
	    (make-segment bl tl))) frame)))

(define (simple-segment frame)
  ((segments->painter
    (list (make-segment (make-vect 0.0 0.0) 
                        (make-vect 1.0 0.0))))
   frame))

;(define csx (corner-split x-mark 8))
;(define sqw (square-limit wave 4))

;;; EOF
