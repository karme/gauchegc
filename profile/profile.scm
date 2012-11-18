;;; profile
;;;
;;; simple high-level "exact" and explicit profiling via debug macro
;;; note: see also gauche (1) -ptype / -ptime for different approach
;;; and maybe take a look at:
;;; https://en.wikipedia.org/wiki/List_of_performance_analysis_tools
;;;
;;; todo: ideally output would be some existing profile output format
;;; maybe something like valgrind's callgrind format
;;; http://kcachegrind.sourceforge.net/html/CallgrindFormat.html
;;; on the other hand the typical profiler collects less information
;;; than this one for performance reasons
;;;
;;; notes:
;;; - measured times are wallclock times
;;;   (heavy system load, gc times, ... might result in misleading
;;;   results! always take care when interpreting profiling output)
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

(define-module profile
  (use srfi-19)
  (use gauche.parameter)
  (export debug-print
          profile-global-hack))

(select-module profile)

(define *call-stack* (make-parameter '()))

(define (time->string t)
  (date->string (time-utc->date t) "~1 ~T.~N~z"))

;; todo: simpler way to detect wether object is serializable?
(define (serializable? x)
  (guard (e [else
             #f])
         (isomorphic? (read-from-string (write/ss-to-string x))
                      x)))

(define-syntax debug-print
  (syntax-rules ()
    ((_ ?form)
     (let ((si (debug-source-info '?form))
           (start-time (current-time)))
       (let ((ci `((source ,(debug-source-info '?form))
                   (pid ,(sys-getpid))
                   (form (,'?form))
                   (stack ,(with-module profile (*call-stack*))))))
         (format/ss (current-error-port)
                    "~s\n"
                    (cons `(time ,(time->string start-time))
                          ci))
         (parameterize ([(with-module profile *call-stack*) (cons (list (car (assoc-ref ci 'source)) '?form)
                                                                  (with-module profile (*call-stack*)))])
           (guard (e [else
                      (let ((end-time (current-time)))
                        (format/ss (current-error-port)
                                   "~s\n"
                                   (append (cons `(time ,(time->string end-time))
                                                 ci)
                                           `((runtime ,(time->seconds (time-difference end-time start-time)))
                                             (error ,(ref e 'message)) ;; todo: only available if error object
                                             ))))
                      (raise e)])
                  (receive vals ?form
                    (let ((end-time (current-time)))
                      (format/ss (current-error-port)
                                 "~s\n"
                                 (append (cons `(time ,(time->string end-time))
                                               ci)
                                         `((runtime ,(time->seconds (time-difference end-time start-time))))
                                         (if (serializable? vals)
                                           `((results ,vals))
                                           '())))
                      (apply values vals))))))))))

;; global change to debug print? uh!
(define (profile-global-hack)
  (let1 x debug-print 
    (with-module gauche.vm.debugger (set! debug-print x))))
