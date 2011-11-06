;;;
;;; runtime compilation / loading of c(ise)-code
;;;
;;;   Copyright (c) 2011 Jens Thiele <karme@karme.de>
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
;; - using gauche 0.9 modules marked as *EXPERIMENTAL* => maybe will
;;   break with other versions!
;; - assumes current directory is in load-path
;; - each compilation loads a module that is never unloaded
;; - likely doesn't work on windows
;; - there is also dyncomp
;;   <http://www.koguro.net/prog/dyncomp/index.html>
;;   using tiny c compiler (tcc) for compilation
(define-module runtime-compile
  (use gauche.package.compile)
  (use gauche.cgen.precomp)
  (use gauche.cgen.tmodule)
  (use file.util)
  (use srfi-27)
  (export cise-compile-and-load
          compile-and-load))

(select-module runtime-compile)

(random-source-randomize! default-random-source)

(define (random-string)
  (format "~x" (random-integer #xfffffffff)))

(define (random-symbol)
  (string->symbol (string-append "grtc" (random-string))))

(define (eprint . l)
  (with-output-to-port (current-error-port)
    (cut apply print l)))

;; todo: name is somewhat misleading/ambiguous / should be in file.util?
(define (with-temporary-directory thunk)
  (define (mk-random-tmpdir)
    (let loop ((try 0))
      (guard (e
              [(<system-error> e)
               (if (< try 10000)
                 (loop (+ try 1))
                 (raise e))])
             (let1 r (string-append (temporary-directory)
                                    "/grtc" ;; todo: add pattern keyword
                                    (random-string))
               (sys-mkdir r #o0700)
               r))))
  
  (let1 dir (mk-random-tmpdir)
    (unwind-protect
     (thunk dir)
     (guard (e
             [else
              (eprint "rmdir " dir " failed") ;; todo
              ])
            (sys-rmdir dir)))))

(define (with-current-directory name thunk)
  (let1 old-dir (current-directory)
    (unwind-protect
     (begin
       ;; fake make to let emacs know we changed directory
       (eprint #`"runtime-compile: Entering directory `,|name|'")
       (current-directory name)
       (thunk))
     (begin
       (eprint #`"runtime-compile: Leaving directory `,|name|'")
       (current-directory old-dir)))))

;; (define (cat x)
;;   (copy-port (open-input-file x) (current-output-port)))

;; (define (cat-asm c-file)
;;   (gauche-package-compile c-file
;;                           :verbose #t
;;                           :cppflags "-v -fverbose-asm -O3 -S")
;;   (cat (path-swap-extension c-file ".o")))
  
(define (%compile-and-load stub imports module)  
  ;; todo
  (define (pprint x)
    (for-each (cut format #t "~s\n" <>)
              x))

  (define (new-module-name)
    (let loop ((try 0))
      (let1 r (random-symbol)
        (cond
         [(not (find-module r))
          r]
         [(and (find-module r)
               (< try 10000))
          (loop (+ try 1))]
         [else
          (error "couldn't create module name")]))))
  
  (with-temporary-directory
   ;; todo: really change to that directory?! likely bad idea
   ;; note: didn't manage to tell cgen-precompile where to put the c-file
   (cut with-current-directory
        <>
        (lambda()
          (let* ((new-mod  (new-module-name))
                 (scm-file #`",|new-mod|.scm")
                 (c-file   #`",|new-mod|.c")
                 (o-file   #`",|new-mod|.o")
                 (sci-file #`",|new-mod|.sci")
                 ;; todo: on windows this would be dll?
                 (so-file  #`",|new-mod|.so"))
            (with-output-to-file scm-file
              (cut pprint
                   `((define-module ,new-mod
                       (export . ,imports))
                     (select-module ,new-mod)
                     . ,stub)))
            ;; (cat scm-file)
            ;; create .c and .sci from .scm
            ;; hackish workaround for bug? in gauche 0.9.2
            (when (not (null? (all-tmodules)))
              (class-slot-set! (with-module gauche.cgen.precomp (current-tmodule-class))
                               'modules
                               (list)))
            (cgen-precompile scm-file :ext-initializer #t)
            ;; compile .so
            (gauche-package-compile-and-link new-mod
                                             (list c-file)
                                             :verbose #t
                                             ;; todo: better use default
                                             :cppflags "-O3 -pipe")
            ;; (cat sci-file)
            (for-each (cut eval <> module)
                      `((load ,sci-file)
                        (import ,|new-mod|)))
            (for-each sys-unlink (list c-file sci-file scm-file))
            ;; todo: on windows you can't delete libraries in use?
            (gauche-package-clean new-mod (list c-file))
            new-mod)))))

;; todo:
;; - be closer to c-wrapper c-load api?
(define-macro (cise-compile-and-load stub imports)
  `(compile-and-load (list (cons 'inline-stub ,stub)) ,imports))

;; todo:
;; - be closer to c-wrapper c-load api?
(define-macro (compile-and-load stub imports)
  `((with-module runtime-compile %compile-and-load)
    ,stub
    ,imports (current-module)))
