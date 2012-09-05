;;; verbose debug-print
;;;
;;;   Copyright (c) 2010-2012 Jens Thiele <karme@karme.de>
;;;   based on debugger.scm
;;;   Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
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


(define-module debug-verbose
  (use srfi-19)
  (export debug-print
          debug-verbose-global-hack))

(select-module debug-verbose)

(define-syntax debug-print
  (syntax-rules ()
    ((_ ?form)
     (let ((si (let1 si (debug-source-info '?form)
                 (if si
                   (string-append (car si)
                                  ":"
                                  (x->string (cadr si)) ": " )
                   "")))
           (ci `((pid . ,(sys-getpid))
                 (form . (,'?form)))))
       (format/ss (current-error-port)
                  "~a~a ~s\n"
                  si
                  (date->string (current-date) "~1 ~T.~N~z")
                  ci)
       (receive vals ?form
         (format/ss (current-error-port)
                    "~a~a ~s\n"
                    si
                    (date->string (current-date) "~1 ~T.~N~z")
                    (append ci
                            `((result . ,vals))))
         (apply values vals))))))

;; global change to debug print? uh!
(define (debug-verbose-global-hack)
  (let1 x debug-print 
    (with-module gauche.vm.debugger (set! debug-print x))))
