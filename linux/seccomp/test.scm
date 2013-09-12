#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../gauchegc/runtime-compile/ -- $0 "$@"
(use gauche.process)
(use runtime-compile)
(use gauche.uvector)
(compile-and-load
 `((inline-stub
    (declcode
     (.include <seccomp.h>)
     "static ScmClass *seccomp_ctx_class = NULL;")
    
    (define-cfn seccomp-ctx-print (h p::ScmPort* c::ScmWriteContext*)
      ::void :static
      (Scm_Printf p
                  "#<seccomp-ctx @%p->%p>"
                  h
                  (SCM_FOREIGN_POINTER_REF scmp_filter_ctx h)))
    
    (define-cfn seccomp-ctx-cleanup (p) ::void :static
      (unless (SCM_XTYPEP p seccomp_ctx_class) (SCM_TYPE_ERROR p "<seccomp-ctx>"))
      (seccomp_release (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p)))
    (define-cproc seccomp-init ()
      (let* ((r::scmp_filter_ctx (seccomp_init
				  SCMP_ACT_KILL
				  ;;SCMP_ACT_TRAP
				  ;;(SCMP_RET_TRACE 0)
				  )))
        ;;(printf "%p\n" r)
        (return (Scm_MakeForeignPointer seccomp_ctx_class r))))
    (define-cproc seccomp-load (p) ::<integer>
      (unless (SCM_XTYPEP p seccomp_ctx_class) (SCM_TYPE_ERROR p "<seccomp-ctx>"))
      ;;(printf "%p\n" (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p))
      (return (SCM_MAKE_INT (seccomp_load (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p)))))

    (define-cproc seccomp-rule-add-exact (p syscall) ::<integer>
      (unless (SCM_XTYPEP p seccomp_ctx_class) (SCM_TYPE_ERROR p "<seccomp-ctx>"))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS exit) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS exit_group) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS rt_sigreturn) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS read) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS write) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS close) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS rt_sigprocmask) 0)
        (return 1))
      (when (seccomp_rule_add_exact (SCM_FOREIGN_POINTER_REF scmp_filter_ctx p) SCMP_ACT_ALLOW (SCMP_SYS brk) 0)
        (return 1))
      (return 0))
    
    (initcode (= seccomp_ctx_class (Scm_MakeForeignPointerClass
                                    (Scm_CurrentModule)
                                    "<seccomp-ctx>" seccomp-ctx-print seccomp-ctx-cleanup
                                    SCM_FOREIGN_POINTER_KEEP_IDENTITY)))
    ))
 '(seccomp-init seccomp-load seccomp-rule-add-exact)
 :cflags (process-output->string '(pkg-config libseccomp --cflags))
 :libs (process-output->string '(pkg-config libseccomp --libs)))

(define (main args)
  (let ((ctx (seccomp-init)))
    (print "hello world!")
    (seccomp-rule-add-exact ctx 'exit_group)
    (when (< #?=(seccomp-load ctx) 0)
      (error "load failed - kernel too old?"))
    ;;(gc)
    (print "hello world!")
    )
  (gc)
  (gc)
  (dotimes (i 100) (make-u8vector (+ i 100000)))
  (gc)
  (gc)
  (print "bye bye!")
  ;; (with-output-to-file "/tmp/foo"
  ;;   (cute print "hehe"))
  ;; (sys-unlink "/tmp/foo")
  0)
