(define-module ode
  (use c-wrapper)
  (use srfi-1)
  (use runtime-compile)
  (use gauche.sequence)
  (export dInitODE2
          dWorldCreate
          dHashSpaceCreate
          dJointGroupCreate
          dGeomGetBody
          dGeomSetBody
          dAreConnected
          dJointAttach
          dJointCreateContact
          dWorldSetGravity
          dCreatePlane
          dJointCreateBall
          dJointAttach
          dJointSetBallAnchor
          dBodyCreate
          dBodyAddForce
          dCreateSphere
          dMassSetBox
          dMassAdjust
          <dMass>
          dBodySetMass
          dBodySetPosition
          get-ode-body-pos
          get-ode-body-rotation
          space-collide
          dWorldStep
          dWorldQuickStep
          dJointGroupEmpty
          dJointGroupDestroy
          dSpaceDestroy
          dWorldDestroy
          dCloseODE
          ode-object-collide))

(select-module ode)

(c-load '("ode/ode.h")
        ;; -DdNODEBUG to make c-wrapper parser happy!
        :cppflags-cmd "echo $(pkg-config --cflags ode) -DdNODEBUG"
        :libs-cmd "pkg-config --libs ode"
        :module #f)

(compile-and-load
 '((inline-stub
    (declcode
     "#define dDOUBLE" ;; todo: in fact depends on pkg-config --cflags ode
     (.include |<ode/ode.h>|))

    "static ScmClass *geomidClass = NULL;"
    ;; "static dWorldID world;"
    ;; "static dSpaceID space;"
    ;; "static dJointGroupID contactgroup;"

    (define-cfn handle-print (obj p::ScmPort* c::ScmWriteContext*) ::void :static
      (Scm_Printf p "#<geomid @%p>" obj))
    
    (initcode
     (= geomidClass
        (Scm_MakeForeignPointerClass
         (SCM_CURRENT_MODULE) ;; note: we assume current module is just compiled module
         "<geomid>"
         handle-print
         NULL ;; handle-cleanup
         SCM_FOREIGN_POINTER_KEEP_IDENTITY)))
    
    (define-cfn unbox (dest::void* obj::ScmObj)
      ::void
      (memcpy dest
              (SCM_UVECTOR_ELEMENTS (Scm_ApplyRec (SCM_SYMBOL_VALUE "c-wrapper.c-ffi" "buffer-of")
                                                  (SCM_LIST1 obj)))
              (sizeof dest)))

    (define-cfn unboxed (obj::ScmObj)
      ::void*
      (let* ((dest::void*))
        (memcpy (& dest)
                (SCM_UVECTOR_ELEMENTS (Scm_ApplyRec (SCM_SYMBOL_VALUE "c-wrapper.c-ffi" "buffer-of")
                                                    (SCM_LIST1 obj)))
                (sizeof dest))
        (return dest)))

    (define-cfn nearCallback (scmObj::void* o1::dGeomID o2::dGeomID)
      ::void
      ;; (printf "scmObj=%p o1=%p o2=%p\n" scmObj o1 o2)
      ;; (printf "world=%p space=%p contactgroup=%p\n" world space contactgroup)
      ;; (printf "world=%p space=%p contactgroup=%p\n"
      ;; (printf "sizeof(dContactGeom)=%d\n" (sizeof dContactGeom))
      (Scm_ApplyRec2 (SCM_OBJ scmObj)
                     (Scm_MakeForeignPointer geomidClass o1)
                     (Scm_MakeForeignPointer geomidClass o2))
      (return)
      ;; cise-version of test callback
      ;; (let* ((b1::dBodyID (dGeomGetBody o1))
      ;;        (b2::dBodyID (dGeomGetBody o2))
      ;;        (contact::dContact))
      ;;   (cond [(and b1 b2 (dAreConnected b1 b2))
      ;;          ;; (printf "connected %p %p\n" b1 b2)
      ;;          (return)]
      ;;         [else
      ;;          (set! (ref contact surface mode) 0) ;;dContactBounce)
      ;;          (set! (ref contact surface mu) 0.1)
      ;;          (set! (ref contact surface mu2) 0)
      ;;          ;; (printf "collide: %p %p %d\n" o1 o2 (sizeof dContactGeom))
      ;;          (when (dCollide o1 o2 1 (& (ref contact geom)) (sizeof dContactGeom))
      ;;            ;; (printf "collision: %p %p\n" o1 o2)
      ;;            (let* ((c::dJointID (dJointCreateContact world contactgroup (& contact))))
      ;;              (dJointAttach c b1 b2)))]))
      ;; (return)
      )

    (define-cproc space-collide-2 (_space _world _contactgroup callback)
      ::<void>
      ;; (unbox (& space) _space)
      ;; (unbox (& world) _world)
      ;; (unbox (& contactgroup) _contactgroup)
      (dSpaceCollide (unboxed _space) callback nearCallback))))
 '(space-collide-2))

(define (space-collide space world contactgroup callback)
  (space-collide-2 space world contactgroup
                   (lambda(o1 o2)
                     (callback (cast <dGeomID> o1)
                               (cast <dGeomID> o2)))))

(define (get-ode-body-pos body)
  (let1 p (make <dVector3>)
    ;; todo: locking?!
    (dBodyCopyPosition body p)
    (coerce-to <list> p)))

(define (get-ode-body-rotation body)
  (let1 r (make <dMatrix3>)
    (dBodyCopyRotation body r)
    (coerce-to <list> r)))

(define ode-object-collide
  (let ((contact (make <dContact>))
        (csize (c-sizeof <dContactGeom>)))
    (set! (ref* contact 'surface 'mode) 0)
    (set! (ref* contact 'surface 'mu) 0.1)
    (set! (ref* contact 'surface 'mu2) 0)
    (lambda(o1 o2)
      (if (not (zero? (dCollide o1 o2 1 (ptr (ref contact 'geom)) (c-sizeof <dContactGeom>))))
        contact
        #f))))
