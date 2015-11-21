#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
# GC_ENABLE_INCREMENTAL=1
# export GC_ENABLE_INCREMENTAL
# GC_PRINT_STATS=1
# export GC_PRINT_STATS
# GC_DONT_GC=1
# export GC_DONT_GC
# GC_PAUSE_TIME_TARGET=5
# export GC_PAUSE_TIME_TARGET
# GC_MAXIMUM_HEAP_SIZE=1000000000
# export GC_MAXIMUM_HEAP_SIZE
# GC_NPROCS=2
# export GC_NPROCS
# GC_MARKERS=2
# export GC_MARKERS
exec gosh -I. -I../runtime-compile -- $0 "$@"
|#

(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use gauche.threads)
(use ode)
(use srfi-1)
(use c-wrapper)
(use srfi-19)

(define-constant RADIUS 0.1732)
(define-constant SIDE 0.2)
(define-constant MASS 1.0)
(define-constant CHAIN 10)
(define-constant TPF (/. 1 60))

(define (timestamp)
  (current-time 'time-monotonic))

(define (timestamp-diff a b)
  (let1 d (time-difference a b)
    (+ (time-second d) (* 1e-9 (time-nanosecond d)))))

(define (ode-create-sphere world space pos radius)
  (let ((rb (dBodyCreate world))
        (rs (dCreateSphere space radius)))
    (apply dBodySetPosition (cons rb pos))
    (let1 m (make <dMass>)
      (dMassSetBox (ptr m) 1 SIDE SIDE SIDE)
      (dMassAdjust (ptr m) MASS)
      (dBodySetMass rb (ptr m))
      (dGeomSetBody rs rb))
    (list rb rs)))

(define (ode-body->matrix body)
  (let ((p (get-ode-body-pos body))
        (r (get-ode-body-rotation body)))
    ;; todo: correct?
    (matrix4f (ref r 0) (ref r 4) (ref r  8) 0
              (ref r 1) (ref r 5) (ref r  9) 0
              (ref r 2) (ref r 6) (ref r 10) 0
              (ref p 0) (ref p 1) (ref p  2) 1)))

(define (anaglyph-paint paint)
  (lambda()
    (gl-color-mask #t #f #f #t)
    (gl-push-matrix*
     (gl-translate -0.02 0 0)
     (paint))
    (gl-color-mask #f #t #t #t)
    (gl-push-matrix*
     (gl-translate 0.02 0 0)
     (paint))
    ;; todo: restore
    (gl-color-mask #t #t #t #t)))

(define (main args)
  (dInitODE2 0)
  (let ((world (dWorldCreate))
        (space (dHashSpaceCreate 0))
        (contactgroup (dJointGroupCreate 1000000)))
    (let1 collide-callback (lambda (o1 o2)
                             (let ((b1 (dGeomGetBody o1))
                                   (b2 (dGeomGetBody o2)))
                               (cond [(and (not (null-ptr? b1))
                                           (not (null-ptr? b2))
                                           (not (zero? (dAreConnected b1 b2))))
                                      ;; nothing to do
                                      (undefined)]
                                     [else
                                      (let1 contact (ode-object-collide o1 o2)
                                        (when contact
                                          (dJointAttach (dJointCreateContact world contactgroup (ptr contact))
                                                        b1 b2)))])))
      (dWorldSetGravity world 0 -0.981 0) ;; -0.5)
      (dCreatePlane space 0 1 0 0)
      (let1 bodies
          (append (map (lambda(i)
                         (let1 k (*. i SIDE)
                           (ode-create-sphere world space (list k (+ k 0.4) k) RADIUS)))
                       (iota CHAIN))
                  (map (lambda(i)
                         (ode-create-sphere world space (list (*. (cos (* 0.01 i)) 0.01)
                                                              i
                                                              (*. (sin (* 0.01 i)) 0.01))
                                            RADIUS))
                       (iota 10)))
        ;; build chain
        (map (lambda(i)
               (let1 joint (dJointCreateBall world 0)
                 (dJointAttach joint (ref* bodies i 0) (ref* bodies (+ i 1) 0))
                 (let1 k (* (+ i 0.5) SIDE)
                   (dJointSetBallAnchor joint k (+ k 0.4) k))
                 joint))
             (iota (- CHAIN 1)))
        
        (glut-init (list))
        #?=simple-viewer-display
        (simple-viewer-display
         #?=(let1 paint (lambda()
                          (for-each
                           (lambda(b)
                             (gl-push-matrix*
                              (gl-mult-matrix
                               ;; todo: locking?!
                               (ode-body->matrix (ref* b 0)))
                              (glut-wire-sphere RADIUS 10 5)
                              ;;(glut-solid-sphere RADIUS 10 10)
                              ))
                           bodies))
              (anaglyph-paint paint)))
        (simple-viewer-window 'demo)
        (let ((angle 0)
              (last-stamp (timestamp))
              ;; todo: this really works?!
              (viewer-thread (make-thread simple-viewer-run #f)))
          (thread-start! viewer-thread)
          (while (not (eq? (thread-state viewer-thread)
                           'terminated))
            (set! angle (+ angle 0.005))
            ;;(when (< i 50)
            ;; apply some force to the root of the chain
            (let1 p (get-ode-body-pos (ref* bodies 0 0))
              (dBodyAddForce (ref* bodies 0 0)
                             (-. (*. 8 (sin angle))
                                 (*. 10 (ref p 0)))
                             8.0
                             (-. (*. 8 (cos angle))
                                 (*. 10 (ref p 2)))))
            ;; force to center of scene
            (let1 p (get-ode-body-pos (ref (last bodies) 0))
              (dBodyAddForce (ref (last bodies) 0)
                             (*. -0.1 (ref p 0))
                             0
                             (*. -0.1 (ref p 2))))
            ;; follow other body
            (let ((p (map (compose get-ode-body-pos (cut ref <> 0)) (take-right bodies 2))))
              (dBodyAddForce (ref* bodies (- (size-of bodies) 2) 0)
                             (-. (ref* p 1 0) (ref* p 0 0))
                             0
                             (-. (ref* p 1 2) (ref* p 0 2))))
            ;; )
            (space-collide space world contactgroup collide-callback)
            ;;(dWorldStep world TPF)
            (dWorldQuickStep world TPF)
            (glut-post-redisplay)
            ;; todo: crap
            (let* ((new-stamp (timestamp))
                   (tosleep (- TPF (timestamp-diff new-stamp last-stamp))))
              (cond [(> tosleep 0.06)
                     #?=(gc)]
                    [(> tosleep 0.001)
                     (thread-sleep! tosleep)])
              (set! last-stamp new-stamp))
            (dJointGroupEmpty contactgroup)
            ))))
      
    (dJointGroupDestroy contactgroup)
    (dSpaceDestroy space)
    (dWorldDestroy world)
    (dCloseODE))
  0)
