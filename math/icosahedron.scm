#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../mercator -I../gl-texture -I../sdl -- $0 "$@"
(use sxml.adaptor) ;; for assert
(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use gl-texture)
(use gauche.collection)
(use srfi-1)
(use srfi-4)
(use gauche.sequence)
(use mercator)
(use gauche.array)
(use math.const)

(debug-print-width 2000)
(define (hero x a)
  (/ (+ x (/ a x)) 2))

(define (approximate-sqrt a prec)
  (let1 r (let loop ((x (/ a 2)))
            (let1 x₊₁ (hero x a)
              (if (and (> x x₊₁) (< (- x x₊₁) prec)) ;; todo: is that correct?!
                x₊₁
                (loop x₊₁))))
    (assert (< (abs (- a (* r r))) prec))
    r))

(define √ sqrt)
;;(define √ (cute approximate-sqrt <> 1/10000000000000000000000))
(define (sqr x) (* x x))
(define ² sqr)
(define φ (/ (+ 1 (√ 5)) 2))
;; ((5^(1/2)+1)/2)/((1+((5^(1/2)+1)/2)^2)^(1/2))
;; ((sqrt(5)+1)/2)/sqrt(1+((sqrt(5)+1)/2)^2)
;; .85065080835203993218154049706301107224040140376481688183674024...
(define (norm x) (√ (apply + (map ² x))))
(define ‖⋅‖ norm)
(define (normalize x)
  (let1 l (‖⋅‖ x)
    (map (cute / <> l) x)))
(define x (/ 1 (‖⋅‖ `(1 ,φ))))
(define y (/ φ (‖⋅‖ `(1 ,φ))))

(define (plane-line-intersection-gl a b c p r)
  (matrix4f-mul
   (matrix4f-inverse
    (make-matrix4f (list->f32vector (append (append r '(0))
                                            (append (vec- a b) '(0))
                                            (append (vec- a c) '(0))
                                            (list 0 0 0 1)))))
   (apply vector4f (vec- a p))))

(define (plane-line-intersection-scm a b c p r)
  (assert (list? p))
  (assert (list? r))
  (assert (list? (vec- a b)))
  (assert (list? (vec- a c)))
  (array->vector (array-mul
                  (array-inverse
                   (array-transpose (apply array (cons (shape 0 3 0 3) (append r
                                                                               (vec- a b)
                                                                               (vec- a c))))))
                  (apply array (cons (shape 0 3 0 1) (vec- a p))))))

(define plane-line-intersection plane-line-intersection-scm)
  
(define (triangle-line-intersection a b c p r)
  (guard (e
          [else
           #f])
         (let* ((x (plane-line-intersection a b c p r))
                (u (ref x 1))
                (v (ref x 2)))
           (list (ref x 0)
                 (- 1 u v)
                 u
                 v))))

(define (triangle-line-intersection? a b c p r)
  (if-let1 l (triangle-line-intersection a b c p r)
    (every (cute >= <> 0) l)
    #f))

(define v (map (lambda(v)
                 (map-to <vector> (lambda(s)
                                    (identity ;; inexact
                                     (case s
                                       [(0) 0]
                                       [(-x) (- x)]
                                       [(x) x]
                                       [(-y) (- y)]
                                       [(y) y])))
                         v))
               '((-x  0 -y)
                 ( x  0 -y)
                 ( x  0  y)
                 (-x  0  y)
                 (-y -x  0)
                 (-y  x  0)
                 ( y  x  0)
                 ( y -x  0)
                 ( 0 -y  x)
                 ( 0 -y -x)
                 ( 0  y -x)
                 ( 0  y  x))))

(define f '((1 9 0)
            (10 1 0)
            (5 10 0)
            (4 5 0)
            (9 4 0)
            (8 2 3)
            (4 8 3)
            (5 4 3)
            (11 5 3)
            (2 11 3)
            (11 2 6)
            (10 11 6)
            (1 10 6)
            (7 1 6)
            (2 7 6)
            (11 10 5)
            (9 8 4)
            (7 2 8)
            (9 7 8)
            (1 7 9)))

(define vec+ (cute map + <...>))
(define vec- (cute map - <...>))
(define (vec* v s) (map (cute * <> s) v))

(define (subdivide a b c depth)
  (cons (list a b c)
        (cond [(> depth 0)
               (assert (every vector? (list a b c)))
               (assert (= (size-of a) (size-of b)))
               (assert (= (size-of a) (size-of c)))
               (receive (ab bc ca)
                   (apply values
                          (map (compose list->vector normalize (cute vec* <> 1/2) vec+)
                               (list a b c)
                               (list b c a)))
                 (map (lambda(x) (apply subdivide (append x (list (- depth 1)))))
                      `((,a ,ab ,ca)
                        (,ab ,b ,bc)
                        (,bc ,c ,ca)
                        (,ab ,bc, ca))))]
              [else
               '()])))
  
;; (define (s l) (apply spherical (map (cute * <> default-radius) l)))
;;#?=(/ (apply great-circle-distance (map (compose (cute subseq <> 0 2) s) (list (cadr vl) (caddr vl)))) 1000)
;;(map (lambda(a b) #?=(/ (* (acos (dot-product a b)) default-radius) 1000)) vl (append (subseq vl 1 3) (list (car vl))))

(define (gl-compile-proc x)
  (let ((l (gl-gen-lists 1)))
    (gl-new-list l GL_COMPILE)
    (x)
    (gl-end-list)
    (lambda()
      (gl-call-list l))))

(define (xyz-togl v)
  (list (ref v 0)
        (ref v 2)
        (- (ref v 1))))

(define (xyz-togl⁻¹ v)
  (list (ref v 0)
        (- (ref v 2))
        (ref v 1)))

(define (gl-to-tex-coord v)
  (spherical->tex-coord
   (apply cartesian->spherical (append (xyz-togl⁻¹ v) (list 1)))))

(define (spherical->tex-coord l)
  (let ((x (car l))
        (y (cadr l)))
    ;; (assert (>= x (- pi)))
    ;; (assert (<= x pi))
    (assert (<= y pi/2))
    (assert (>= y (- pi/2)))
    (let ((rx (/ (+ x pi) (* 2 pi)))
          (ry (- 1 (/ (+ y pi/2) pi))))
      (assert (and (>= rx 0) (<= rx 1)))
      (assert (and (>= ry 0) (<= ry 1)))
      (list rx ry))))

(define (tex-coord->spherical tc)
  (list (* (- (car tc) 1/2) 2 pi)
        (- (* (- 1 (cadr tc)) pi) pi/2)))

(define (draw-sphere-2 subdivision-depth color)
  (gl-material GL_FRONT GL_SPECULAR '#f32(0.0 0.0 0.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 50.0)
  (for-each-with-index (lambda(i x)
                         (assert (number? i))
                         (assert (list? x))
                         (let ((current-color (lambda()
                                                (if (not (zero? color))
                                                  (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE
                                                               (f32vector (+ (* (/ (modulo (+ i 0) 11) 11) 3/4) 1/4)
                                                                          (+ (* (/ (modulo (+ i 3) 7) 7) 3/4) 1/4)
                                                                          (+ (* (/ (modulo (+ i 2) 3) 3) 3/4) 1/4)
                                                                          1))
                                                  (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE (f32vector 1 1 1 1))
                                                  ))))
                           (current-color)
                           ((rec (f x search)
                                 (cond [(null? (cdr x))
                                        (let1 selected (not (null?
                                                             (filter (lambda(s)
                                                                       (apply triangle-line-intersection?
                                                                              (append (car x)
                                                                                      (list '(0 0 0)
                                                                                            s))))
                                                                     search)))
                                          (when selected
                                            (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE
                                                         (f32vector 1
                                                                    0
                                                                    0
                                                                    1)))
                                          
                                          (case color
                                            [(0 1)
                                             (gl-enable GL_TEXTURE_2D)
                                             (gl-bind-texture GL_TEXTURE_2D (assoc-ref *texture* 'name))
                                             (gl-begin GL_TRIANGLES)
                                             (let1 last-tex-coord #f
                                               (for-each (lambda(v)
                                                           (let1 f32v (vector->f32vector v)
                                                             (gl-normal f32v)
                                                             ;; ugly hack for wrap-around
                                                             ;; works ok for higher resolutions
                                                             (let1 tc (gl-to-tex-coord v)
                                                               (let1 tc (cond [last-tex-coord
                                                                               #?=(norm (vec- tc last-tex-coord))
                                                                               #?=(norm (vec- (vec+ tc '(1 0))
                                                                                              last-tex-coord))
                                                                               #?=(norm (vec- (vec+ tc '(-1 0))
                                                                                              last-tex-coord))
                                                                               (map(lambda(d x)
                                                                                     (cond [(> d 0.5)
                                                                                            (- x 1)]
                                                                                           [(< d -0.5)
                                                                                            (+ x 1)]
                                                                                           [else
                                                                                            x]))
                                                                                   (vec- tc last-tex-coord)
                                                                                   tc)]
                                                                              [else
                                                                               tc])
                                                                 (apply gl-tex-coord tc)
                                                                 (set! last-tex-coord tc)))
                                                             (gl-vertex f32v)))
                                                         (car x)))
                                             (gl-end)
                                             (gl-disable GL_TEXTURE_2D)]
                                            [else
                                             (gl-begin GL_TRIANGLES)
                                             (for-each (lambda(v)
                                                           (let1 f32v (vector->f32vector v)
                                                             (gl-normal f32v)
                                                             (gl-vertex f32v)))
                                                       (car x))
                                             (gl-end)])
                                          (when selected
                                            ;; todo: no push/pop for material?!
                                            (current-color)))]
                                       [else
                                        (assert (list? x))
                                        (for-each (cute f <> (filter (lambda(s)
                                                                       (apply triangle-line-intersection?
                                                                              (append (car x)
                                                                                      (list '(0 0 0)
                                                                                            s))))
                                                                     search))
                                                  (cdr x))]))
                            x
                            (map (lambda(x) (xyz-togl (apply spherical->cartesian
                                                             (map deg->rad x))))
                                 '((9 48.5)
                                   (0 0) ;; todo: float precision troubles
                                   ;;(90 45)
                                   )))))
                       (map (lambda(t)
                              (apply subdivide (append (map (cute ref v <>) t)
                                                       (list subdivision-depth))))
                            f)))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (fn . arg) body ...)
     (define-memoized fn (lambda arg body ...)))
    ((_ fn lambda-body)
     (define fn (let ((cache (make-hash-table 'equal?))
                      (rawfn lambda-body))
                  (lambda args
                    (if (hash-table-exists? cache args)
                      (apply values (hash-table-get cache args))
                      (receive val
                          (apply rawfn args)
                        (hash-table-put! cache args val)
                        (apply values val)))))))))

(define-memoized (get-draw-sphere subdivision-depth color)
  (gl-compile-proc
   (lambda()
     (draw-sphere-2 subdivision-depth color))))

(define (draw-sphere subdivision-depth color)
  (when (not *texture*)
    (set! *texture* (gl-texture-load "marble.jpg")))
  ((get-draw-sphere subdivision-depth color)))

(define (sec-of-day)
  (let1 t (sys-gmtime (current-time))
    (apply + (map (lambda(x y)
                    (* (ref t x) y))
                  '(hour min sec)
                  '(3600 60 1)))))
(define (sun-position)
  (let1 a (+ (* 2 pi (/ (sec-of-day)
                        (* 24 3600)))
             pi)
    (list (cos a)
          (sin a)
          0 ;; todo
          )))

(define *texture* #f)

(define (main args)
  (glut-init args)
  (let ((depth 0)
        (grid 0)
        (light 0)
        (color 0))
    (simple-viewer-display
     (lambda()
       ;;(gl-clear-color 1 1 1 0)
       (gl-enable GL_CULL_FACE)
       ;;(gl-cull-face GL_BACK)
       (gl-light-model GL_LIGHT_MODEL_AMBIENT (f32vector 0.3 0.3 0.3 1))
       (gl-shade-model GL_SMOOTH)
       ;; (gl-material GL_FRONT GL_AMBIENT (f32vector 1.0 1.0 1.0 1.0))
       ;; (gl-material GL_FRONT GL_DIFFUSE (f32vector 1.0 0.5 0.5 1.0))
       
       (gl-enable GL_LIGHTING)
       (case light
         [(0)
          (gl-enable GL_LIGHT0)
          (gl-light GL_LIGHT0 GL_POSITION (apply f32vector (append (xyz-togl (sun-position)) '(0))))
          ]
         [(1)
          (gl-enable GL_LIGHT0)
          (gl-light GL_LIGHT0 GL_POSITION '#f32(0.0 10.0 0.0 1.0))
          ]
         [else
          (gl-disable GL_LIGHT0)])
       (gl-enable GL_DEPTH_TEST)
       (gl-enable GL_POLYGON_OFFSET_FILL)
       (gl-polygon-offset 1 1)
       (draw-sphere depth color)
       (case grid
         [(0)
          (gl-disable GL_POLYGON_OFFSET_FILL)
          (gl-disable GL_LIGHTING)
          (gl-disable GL_LIGHT0)
          (gl-color 0 0 0)
          (gl-polygon-mode GL_FRONT_AND_BACK GL_LINE)
          (draw-sphere depth color)
          (gl-polygon-mode GL_FRONT_AND_BACK GL_FILL)]
         [(1)
          (gl-push-matrix*
           ;; use backface culling and convexity
           (gl-disable GL_DEPTH_TEST)
           (gl-depth-mask #f)
           (gl-rotate 90 1 0 0)
           (gl-disable GL_LIGHTING)
           (gl-disable GL_LIGHT0)
           (gl-color 0 0 0)
           (gl-disable GL_POLYGON_OFFSET_FILL)
           (gl-polygon-mode GL_FRONT_AND_BACK GL_LINE)
           (glut-solid-sphere 1
                              (* 360/20 (+ depth 1))
                              (* 180/20 (+ depth 1)))
           (gl-polygon-mode GL_FRONT_AND_BACK GL_FILL)
           (gl-depth-mask #t)
           (gl-enable GL_DEPTH_TEST))])))
    (simple-viewer-set-key! #f
                            #\+ (lambda _ (set! depth (modulo (+ depth 1) 7)))
                            #\- (lambda _ (set! depth (modulo (- depth 1) 7)))
                            #\g (lambda _ (set! grid (modulo (+ grid 1) 3)))
                            #\l (lambda _ (set! light (modulo (+ light 1) 3)))
                            #\c (lambda _ (set! color (modulo (+ color 1) 3)))
                            #\q (lambda _ (exit))
                            #\escape (lambda _ (exit)))
    (simple-viewer-window 'test
                          ;;:mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB GLUT_MULTISAMPLE)
                          )
    (simple-viewer-run) ; loop till window close
    )
  0)
