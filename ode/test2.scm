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

;; based on demo_crash
;; todo: adjust collision detection

(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use gauche.threads)
(use ode)
(use srfi-1)
(use c-wrapper)
(use srfi-19)
(use gauche.collection)
(use gauche.sequence)
(use gauche.parameter)
(use math.const)
(use util.queue)

(define-constant TPF (*. (/. 1 60) 1))
(define-constant WALLMASS 1)
(define-constant WBOXSIZE 0.5)
(define-constant DISABLE_THRESHOLD 0.1)
;;(define-constant DISABLE_STEPS 20)
;;(define-constant ITERS 30)

(define *queue* (make-queue))
(define *ball* #f)

(define (timestamp)
  (current-time 'time-monotonic))

(define (timestamp-diff a b)
  (let1 d (time-difference a b)
    (+ (time-second d) (* 1e-9 (time-nanosecond d)))))

;; todo: small scene graph

(define ode-matrix (make-parameter (make-matrix4f)))
(define ode-world  (make-parameter #f))
(define ode-space  (make-parameter #f))

(define (translate t thunk)
  (parameterize ((ode-matrix (* (ode-matrix) (translation->matrix4f t))))
    (thunk)))

(define (rotate axis angle thunk)
  (parameterize ((ode-matrix (* (ode-matrix) (rotation->matrix4f axis angle))))
    (thunk)))

(define (matrix->ode-rotation-matrix m)
  (matrix4f->list (matrix4f-transpose m)))

(define (box . args)
  (let-optionals* args ((lx 1)
                        (ly lx)
                        (lz ly)
                        (mass 1))
    (let ((rb (dBodyCreate (ode-world)))
          (rs (dCreateBox (ode-space) lx ly lz)))
      ;;(dBodySetAutoDisableDefaults rb)
      ;;#?=(dBodyGetAutoDisableFlag rb)
      ;;#?=(dBodyGetAutoDisableSteps rb)
      (receive (flag t r h s)
          (matrix4f-decompose (ode-matrix))
        (apply dBodySetPosition (cons rb
                                      (subseq (vector4f->list t)
                                              0
                                              3)))
        (dBodySetRotation rb (matrix->ode-rotation-matrix r)))
      (let1 m (make <dMass>)
        ;; mass?
        (dMassSetBoxTotal (ptr m) mass lx ly lz)
        (dBodySetMass rb (ptr m))
        (dGeomSetBody rs rb))
      (list rb rs
            (lambda()
              (glut-wire-cube lx) ;; todo
              )))))

(define (box-row num . args)
  (let-optionals* args ((lx 1)
                        (ly lx)
                        (lz ly)
                        (mass 1))
    (translate (list->vector4f (list (- (/. lx 2))
                                     (/. ly 2)
                                     (- (/. (* lz (- num 1)) 2))
                                     0))
               (lambda()
                 (map
                  (lambda(i)
                    (translate (list->vector4f (list 0 0 (*. i lz) 0))
                               (cut box lx ly lz mass)))
                  (iota num))))))

(define (ode-create-box world space pos)
  (parameterize ((ode-world world)
                 (ode-space space))
    (translate (list->vector4f pos)
               (lambda() (box WBOXSIZE)))))

(define (create-wall-2 world space num row)
  (if (<= num 0)
    '()
    (append (map (lambda(i)
                   (ode-create-box world space (list 0
                                                     (*. (+ row 0.5) WBOXSIZE)
                                                     (*. (-. i (/. (- num 1) 2)) WBOXSIZE))))
                 (iota num))
            (create-wall-2 world space (- num 1) (+ row 1)))))

(define (create-wall world space)
  (parameterize ((ode-world world)
                 (ode-space space))
    (append
     (translate (list->vector4f (list -3 4 0 0))
                (lambda()
                  (rotate #,(vector4f 1 0 0 0) pi/2 (lambda()
                                                      (box-row 8)))))
     (translate (list->vector4f (list -1 0 0 0))
                (lambda()
                  (box-row 8)))
     (create-wall-2 world space 8 0))))

(define (ode-create-sphere world space pos radius)
  (let ((rb (dBodyCreate world))
        (rs (dCreateSphere space radius)))
    (apply dBodySetPosition (cons rb pos))
    (let1 m (make <dMass>)
      (dMassSetSphereTotal (ptr m) 2 radius)
      (dBodySetMass rb (ptr m))
      (dGeomSetBody rs rb))
    (list rb rs
          (lambda()
            (glut-wire-sphere radius 10 5)))))

(define (ode-body->matrix body)
  (let ((p (get-ode-body-pos body))
        (r (get-ode-body-rotation body)))
    ;; todo: correct?
    (matrix4f (ref r 0) (ref r 4) (ref r  8) 0
              (ref r 1) (ref r 5) (ref r  9) 0
              (ref r 2) (ref r 6) (ref r 10) 0
              (ref p 0) (ref p 1) (ref p  2) 1)))

(define (norm x)
  (+. (*. (ref x 0) (ref x 0))
      (*. (ref x 1) (ref x 1))
      (*. (ref x 2) (ref x 2))))

(define (main args)
  (dInitODE2 0)
  (let ((world (dWorldCreate))
        (space (dHashSpaceCreate 0))
        (contactgroup (dJointGroupCreate 1000000)))
    (let1 collide-callback (lambda (o1 o2)
                             (let ((b1 (dGeomGetBody o1))
                                   (b2 (dGeomGetBody o2)))
                               (cond [(and (not (null-ptr? b1)) ;; don't add contact between connected bodies
                                           (not (null-ptr? b2))
                                           (not (zero? (dAreConnected b1 b2))))
                                      (undefined)]
                                     [(and (or (and (null-ptr? b2) ;; disabled body doesn't need contact to surface!
                                                    (zero? (dBodyIsEnabled b1)))
                                               (and (null-ptr? b1)
                                                    (zero? (dBodyIsEnabled b2)))))
                                      (undefined)]
                                     [else
                                      (for-each (lambda(contact)
                                                  (when (zero? (ref* (deref contact) 'surface 'mode))
                                                    (set! (ref* (deref contact) 'surface 'mode)
                                                          (logior dContactSlip1 dContactSlip2 dContactSoftERP dContactSoftCFM dContactApprox1))
                                                    (set! (ref* (deref contact) 'surface 'mu) 0.5)
                                                    (set! (ref* (deref contact) 'surface 'slip1) 0.0)
                                                    (set! (ref* (deref contact) 'surface 'slip2) 0.0)
                                                    (set! (ref* (deref contact) 'surface 'soft_erp) 0.8)
                                                    (set! (ref* (deref contact) 'surface 'soft_cfm) 1e-5))
                                                  (dJointAttach (dJointCreateContact world contactgroup contact)
                                                                b1 b2))
                                                (ode-object-collide-2 o1 o2))])))
      (dWorldSetGravity world 0 -9.81 0) ;; -0.5)
      (dWorldSetCFM world 1e-5)
      (dWorldSetERP world 0.8)
      ;;(dWorldSetQuickStepNumIterations world ITERS)
      (dWorldSetAutoDisableFlag world 1)
      (dWorldSetAutoDisableAverageSamplesCount world 10)
      ;;(dWorldSetAutoDisableTime world 2)
      (dWorldSetAutoDisableLinearThreshold world DISABLE_THRESHOLD)
      (dWorldSetAutoDisableAngularThreshold world DISABLE_THRESHOLD)
      (dCreatePlane space 0 1 0 0)
      (let1 bodies (create-wall world space)
        (glut-init (list))
        (simple-viewer-display
         (lambda()
           (for-each
            (lambda(b)
              (gl-push-matrix*
               (gl-mult-matrix
                ;; todo: locking?!
                (ode-body->matrix (ref b 0)))
               (if (not (zero? (dBodyIsEnabled (ref b 0))))
                 (gl-color 1 1 1)
                 (gl-color 0.5 1 0.5))
               ((ref b 2))
               ))
            bodies)))
        (let ((sy 1)
              (on-key (lambda(c proc)
                        (simple-viewer-set-key! #f c (lambda _ (queue-push! *queue* proc))))))
          (on-key #\s (lambda()
                        (when (not *ball*)
                          (set! *ball* (ode-create-sphere world space (list 0 0 0) 0.3))
                          (append! bodies (list *ball*)))
                        (dBodySetPosition  (ref *ball* 0)   5 sy 0)
                        (dBodySetLinearVel (ref *ball* 0) -10  0 0)
                        (dBodyEnable (ref *ball* 0))))
          (on-key #\w (lambda() (inc! sy 0.1)))
          (on-key #\x (lambda() (dec! sy 0.1))))
        (simple-viewer-window 'demo)
        (let ((last-stamp (timestamp))
              ;; todo: this really works?!
              (viewer-thread (make-thread simple-viewer-run #f)))
          (thread-start! viewer-thread)
          (while (not (eq? (thread-state viewer-thread)
                           'terminated))
            (when (not (queue-empty? *queue*))
              ((queue-pop! *queue*)))
            (space-collide space world contactgroup collide-callback)
            ;;(dWorldStep world TPF)
            (dWorldQuickStep world TPF)
            ;; todo: use auto-disable?!
            ;; (for-each (lambda(body)
            ;;             (let1 b (car body)
            ;;               (when (not (zero? (dBodyIsEnabled b)))
            ;;                 (cond [(ode-body-idle? b DISABLE_THRESHOLD)
            ;;                        (inc! (ref body 2))
            ;;                        (when (> (ref body 2) DISABLE_STEPS)
            ;;                          (dBodyDisable b))]
            ;;                       [else
            ;;                        (set! (ref body 2) 0)]))))
            ;;           bodies)
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
