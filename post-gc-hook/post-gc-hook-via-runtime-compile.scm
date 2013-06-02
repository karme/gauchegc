;;;
;;; post gc hook
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(define-module post-gc-hook-via-runtime-compile
  (use runtime-compile)
  (use util.queue)
  (export run-post-gc-hooks
          with-post-gc-hook))

(select-module post-gc-hook-via-runtime-compile)

(compile-and-load
 `((inline-stub
    (define-cfn myfinalize (obj f::void*) ::void :static
      ;;(printf "huhu2\n")
      (Scm_ApplyRec0 f))
    (define-cproc register-finalizer (obj f)
      ;;(printf "huhu1\n")
      (Scm_RegisterFinalizer obj myfinalize f)
      (return obj))))
 '(register-finalizer))

(define *hooks-to-run* (make <mtqueue>))

(define (run-post-gc-hooks)
  (while (queue-pop! *hooks-to-run* #f) => f
         (f)))

(define (with-post-gc-hook f proc)
  (when (not (procedure? f)) (error "sorry"))
  (register-finalizer (lambda l (apply f l))
                      (lambda()
                        (queue-push! *hooks-to-run* proc))))
