#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

;;; small libgccjit test
;;; s.a.
;;; https://gcc.gnu.org/onlinedocs/jit/
;;; https://gcc.gnu.org/wiki/JIT

(sys-setenv "PATH" (string-append (sys-getenv "PATH") ":/usr/lib/gcc-snapshot/bin") #t)
(use c-wrapper)
;;(c-load "/usr/lib/gcc/arm-linux-gnueabihf/5/include/libgccjit.h" :ldflags "-L/usr/lib/arm-linux-gnueabihf" :libs  "-lgccjit.so.0")
(c-load "/usr/lib/gcc-snapshot/lib/gcc/arm-linux-gnueabihf/5.0.0/include/libgccjit.h"
	:ldflags "-L/usr/lib/gcc-snapshot/lib" :libs  "-lgccjit.so")

(define (main args)
  (let1 ctxt (gcc_jit_context_acquire)
	#?=ctxt
	(for-each (lambda(kv)
		    (gcc_jit_context_set_bool_option ctxt
						     (car kv)
						     (cadr kv)))
		  `(
		    ;; (,GCC_JIT_BOOL_OPTION_DEBUGINFO 1)
		    (,GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE 1)
		    (,GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE 1)
		    (,GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE 1)
		    (,GCC_JIT_BOOL_OPTION_DUMP_SUMMARY 1)
		    ;; (,GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING 1)
		    ;; (,GCC_JIT_BOOL_OPTION_SELFCHECK_GC 1)
		    ;; (,GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES 1)
		    (,GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL 3)
		    ))
	(let* ((int-type (gcc_jit_context_get_type ctxt GCC_JIT_TYPE_INT))
	       (param-i (gcc_jit_context_new_param ctxt NULL int-type "i"))
	       (func (gcc_jit_context_new_function ctxt NULL
						   GCC_JIT_FUNCTION_EXPORTED
						   int-type "square" 1 (ptr param-i) 0))
	       (block (gcc_jit_function_new_block func NULL))
	       (expr (gcc_jit_context_new_binary_op ctxt NULL
						    GCC_JIT_BINARY_OP_MULT int-type
						    (gcc_jit_param_as_rvalue param-i)
						    (gcc_jit_param_as_rvalue param-i))))
	  (gcc_jit_block_end_with_return block NULL expr)
	  (let1 result #?=(gcc_jit_context_compile ctxt)
		#?=(gcc_jit_context_release ctxt)
		(let1 fptr (gcc_jit_result_get_code result "square")
		      #?=func
		      #?=(#?=(cast (c-func-ptr <c-int> (list <c-int>)) fptr) 2)
		      (gcc_jit_result_release result)))))
  0)
