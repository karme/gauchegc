;;;
;;; fork-process similar to run-process for gauche >= 0.9.1
;;;
;;;   Copyright (c) 2011 Jens Thiele <karme@karme.de>
;;;   based on code:
;;;   Copyright (c) 2000-2011  Shiro Kawai  <shiro@acm.org>
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

#!no-fold-case

;; notes:
;; - ugly hack
;; - for caveats see also sys-fork
;; - this is for gauche >=0.9
;; - :detached, :host, :fork and are not supported

(define-module fork-process
  (extend gauche.process)
  (use runtime-compile)
  (use srfi-1)
  (use file.util)
  (use gauche.version)
  (export fork-process))

(select-module fork-process)

(define-macro (%check-version)
  (if (version<=? "0.9" (gauche-version))
    '#t
    '(syntax-error "sorry only gauche >= 0.9 supported")))

(cond-expand
 [gauche.os.windows
  (syntax-error "sorry, windows not supported")]
 [else
  (%check-version)])

;; hack to get Scm_SysSwapFds and Scm_SysPrepareFdMap
;; box is only available in > 0.9 :( => foreign pointer
(with-output-to-port (current-error-port)
  (lambda()
    (cise-compile-and-load
     `((declcode
        (.include |<gauche.h>|)
        "static ScmClass *fdmapClass = NULL;")
       ;; (define-cfn handle-cleanup (obj) ::void :static
       ;;   )
       (define-cfn handle-print (obj p::ScmPort* c::ScmWriteContext*) ::void :static
         (Scm_Printf p "#<fdmap @%p>" obj))
       (initcode
        ;; (Scm_Write (SCM_CURRENT_MODULE)
        ;;            (SCM_VM_CURRENT_ERROR_PORT (Scm_VM))
        ;;            SCM_WRITE_WRITE)
        (= fdmapClass
           (Scm_MakeForeignPointerClass
            (SCM_CURRENT_MODULE) ;; note: we assume current module is just compiled module
            "<fdmap>"
            handle-print
            NULL ;; handle-cleanup
            SCM_FOREIGN_POINTER_KEEP_IDENTITY)))
       (define-cproc sys-swap-fds (obj) ::<void>
         (unless (SCM_XTYPEP obj fdmapClass) (SCM_TYPE_ERROR obj "<fdmap>"))
         (Scm_SysSwapFds (SCM_FOREIGN_POINTER_REF (int*) obj)))
       (define-cproc sys-prepare-fdmap (obj)
         (result (Scm_MakeForeignPointer fdmapClass (Scm_SysPrepareFdMap obj)))))
     '(sys-swap-fds sys-prepare-fdmap))))

(define (sys-sigset-inv sigset)
  (sys-sigset-delete! (sys-sigset-fill! (make <sys-sigset>)) sigset))

;; todo: there must be a better way!
(define-macro (ifdef c x)
  (cond [(boolean? c)
         (if c x '#t)]
        [else
         `(ifdef ,(eval c
                        (current-module) ;; ouch
                        ) ,x)]))

(define (clear-process-list!)
  (class-slot-set! <process> 'processes '()))

(ifdef (version=? "0.9" (gauche-version))
       ;; based on sys-fork-and-exec/Scm_SysExec
       (define (sys-fork-and-call thunk . args)
         (let-keywords* args ((iomap ()) (sigmask #f) (directory #f))
           (unless (or (not sigmask) (is-a? sigmask <sys-sigset>))
             (error #`"<sys-sigset> or #f required, but got ,sigmask"))
           (flush-all-ports) ;; don't duplicate output => empty buffers
           (let1 fds (sys-prepare-fdmap iomap)
             (gc)
             (let1 pid (sys-fork)
               (case pid
                 [(0)
                  (clear-process-list!)
                  ;; todo: somehow get unregister_buffered_port called
                  (sys-swap-fds fds)
                  (when sigmask
                    ;; reset signal handlers except the masked ones
                    (set-signal-handler! (sys-sigset-inv sigmask) #t)
                    (sys-sigmask SIG_SETMASK sigmask))
                  (when directory
                    (current-directory directory))
                  (let ((stdin (open-input-fd-port 0
                                                   :buffering :full
                                                   :name "(stdin)"
                                                   :owner?  #t))
                        (stdout (open-output-fd-port 1
                                                     :buffering :full
                                                     :name "(stdout)"
                                                     :owner?  #t))
                        (stderr (open-output-fd-port 2
                                                     :buffering :full
                                                     :name "(stderr)"
                                                     :owner?  #t)))
                    ;; todo: set standard ports!
                    ;;              (standard-input-port stdin)
                    ;;              (standard-output-port stdout)
                    ;;              (standard-error-port stderr)
                    (with-ports stdin stdout stderr thunk)
                    (close-input-port stdin)
                    (close-output-port stdout)
                    (close-output-port stderr))
                  (sys-exit 0)]
                 [else
                  pid]))))))

(ifdef (version=? "0.9" (gauche-version))
       ;; based on and very similar to %run-process-new
       (define (fork-process thunk . args)
         (let-keywords* args ((input  #f) (output #f) (error  #f)
                              (wait   #f)
                              (sigmask #f)
                              (directory #f) ;; todo
                              )
           (%check-iokey :input input)
           (%check-iokey :output output)
           (%check-iokey :error error)
           (let* ([proc (make <process> :command (with-module user
                                                   *program-name*))]
                  [argv (with-module user *argv*)]
                  [dir  #f])
             (receive (iomap toclose)
                 (if (or input output error)
                   (%setup-iomap proc input output error)
                   (values #f '()))
               (let1 pid (sys-fork-and-call thunk
                                            :iomap iomap
                                            :sigmask (%ensure-mask sigmask)
                                            :directory directory)
                 (push! (ref proc 'processes) proc)
                 (set!  (ref proc 'pid) pid)
                 (dolist (p toclose)
                   (if (input-port? p)
                     (close-input-port p)
                     (close-output-port p)))
                 (when wait
                   ;; the following expr waits until the child exits
                   (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
                   (update! (ref proc 'processes) (cut delete proc <>)))
                 proc))))))

(ifdef (version>=? (gauche-version) "0.9.1")
       ;; based on sys-fork-and-exec/Scm_SysExec
       (define (sys-fork-and-call thunk . args)
         (let-keywords* args ((iomap ()) (sigmask #f) (directory #f) (detached #f))
           (unless (or (not sigmask) (is-a? sigmask <sys-sigset>))
             (error #`"<sys-sigset> or #f required, but got ,sigmask"))
           (flush-all-ports) ;; don't duplicate output => empty buffers
           (let1 fds (sys-prepare-fdmap iomap)
             (gc)
             (let1 pid (sys-fork)
               (case pid
                 [(0)
                  (clear-process-list!)
                  ;; todo: somehow get unregister_buffered_port called
                  ;; todo: detached => fork again+setsid
                  (sys-swap-fds fds)
                  (when sigmask
                    ;; reset signal handlers except the masked ones
                    (set-signal-handler! (sys-sigset-inv sigmask) #t)
                    (sys-sigmask SIG_SETMASK sigmask))
                  (when directory
                    (current-directory directory))
                  (let ((stdin (open-input-fd-port 0
                                                   :buffering :full
                                                   :name "(stdin)"
                                                   :owner?  #t))
                        (stdout (open-output-fd-port 1
                                                     :buffering :full
                                                     :name "(stdout)"
                                                     :owner?  #t))
                        (stderr (open-output-fd-port 2
                                                     :buffering :full
                                                     :name "(stderr)"
                                                     :owner?  #t)))
                    ;; set standard ports
                    (standard-input-port stdin)
                    (standard-output-port stdout)
                    (standard-error-port stderr)
                    (with-ports stdin stdout stderr thunk)
                    (close-input-port stdin)
                    (close-output-port stdout)
                    (close-output-port stderr))
                  (sys-exit 0)]
                 [else
                  pid]))))))

(ifdef (and (version>=? (gauche-version) "0.9.1") (version<=? (gauche-version) "0.9.4"))
       ;; based on and very similar to %run-process-new
       (define (fork-process thunk . args)
         (let-keywords* args ((input  #f) (output #f) (error  #f)
                              (redirects '())
                              (wait   #f)
                              (sigmask #f) (directory #f) (detached #f))
           (let* ([redirs (%canon-redirects redirects input output error)]
                  ;; todo
                  [argv (with-module user *argv*)]
                  [proc (make <process>
                          ;; todo
                          :command (with-module user *program-name*))]
                  [dir  directory])
             (%check-directory dir)
             (receive (iomap toclose ipipes opipes)
                 (if (pair? redirs)
                   (%setup-iomap proc redirs)
                   (values #f '() '() '()))
               (set! (~ proc'in-pipes) ipipes)
               (set! (~ proc'out-pipes) opipes)
               (let1 pid (sys-fork-and-call thunk
                                            :iomap iomap :directory dir
                                            :sigmask (%ensure-mask sigmask)
                                            :detached detached)
                 (push! (ref proc 'processes) proc)
                 (set!  (ref proc 'pid) pid)
                 (dolist (p toclose)
                   (if (input-port? p)
                     (close-input-port p)
                     (close-output-port p)))
                 ;; note: detached and wait together don't work!
                 (when (and wait (not detached))
                   ;; the following expr waits until the child exits
                   (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
                   (update! (ref proc 'processes) (cut delete proc <>)))
                 proc))))))

(ifdef (version>=? (gauche-version) "0.9.5")
       ;; based on and very similar to %run-process-new
       (define (fork-process thunk . args)
         (let-keywords* args ((input  #f) (output #f) (error  #f)
                              (redirects '())
                              (wait   #f)
                              (sigmask #f) (directory #f) (detached #f))
           (let* ([redirs (%canon-redirects redirects input output error)]
                  ;; todo
                  [argv (with-module user *argv*)]
                  [proc (make <process>
                          ;; todo
                          :command (with-module user *program-name*))]
                  [dir  directory])
             (%check-directory dir)
             (receive (iomap toclose ipipes opipes tmpfiles)
                 (if (pair? redirs)
                   (%setup-iomap proc redirs)
                   (values #f '() '() '() '()))
               (set! (~ proc'in-pipes) ipipes)
               (set! (~ proc'out-pipes) opipes)
               (let1 pid (sys-fork-and-call thunk
                                            :iomap iomap :directory dir
                                            :sigmask (%ensure-mask sigmask)
                                            :detached detached)
                 (push! (ref proc 'processes) proc)
                 (set!  (ref proc 'pid) pid)
                 (dolist (p toclose)
                   (if (input-port? p)
                     (close-input-port p)
                     (close-output-port p)))
                 ;; note: detached and wait together don't work!
                 (when (and wait (not detached))
                   ;; the following expr waits until the child exits
                   (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
                   (update! (ref proc 'processes) (cut delete proc <>))
		   (for-each sys-unlink tmpfiles))
                 proc))))))
