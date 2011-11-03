#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -I../runtime-compile -- $0 "$@"

(use gauche.test)
(use file.util) ;; null-device
(use gauche.version)

;; not in 0.9?
;; note: doesn't work on windows anyway
(when (not (global-variable-bound? 'file.util 'null-dev))
  (with-module file.util
    (define (null-device)
      (cond-expand [gauche.os.windows "NUL"] [else "/dev/null"]))
    (export null-device)))

(test-start "fork-process")

(use fork-process)
(test-module 'fork-process)

(define (some-sleep) (sys-sleep 1))

(define (wait-exit p)
  (and (process-wait p)
       (sys-wait-exited? (process-exit-status p))
       (sys-wait-exit-status (process-exit-status p))))

(test* "fork-process"
       0
       (wait-exit (fork-process (lambda()))))

(test* "output to file"
       0
       (let1 p (fork-process (cut print "hello") :output "test.o") ;; todo: safe temp file
         (and (process-wait p)
              (with-input-from-file "test.o"
                (cut equal? (read-line) "hello"))
              (process-exit-status p))))

(test* "input from file"
       0
       (let1 p (fork-process (lambda()
                               (unless (equal? (read-line) "hello") (error "failed")))
                             :input "test.o")
         (and (process-wait p)
              (process-exit-status p))))

(test* "sys-exit 1"
       1
       (wait-exit (fork-process (cut sys-exit 1))))

(test* "output pipe"
       (list "hello" 0)
       (let1 p (fork-process (lambda()
                               (display "hello")
                               (flush)) :output :pipe)
         (list (port->string (process-output p))
               (wait-exit p))))

(test* "output pipe (parent stdout !=1)"
       (list "hello" 0)
       (with-output-to-file (null-device)
         (lambda()
           (let1 p (fork-process (lambda()
                                   (display "hello")
                                   (flush)) :output :pipe)
             (list (port->string (process-output p))
                   (wait-exit p))))))

(test* "error pipe"
       (list "hello" 0)
       (let1 p (fork-process (lambda()
                               (with-output-to-port (current-error-port)
                                 (lambda()
                                   (display "hello")
                                   (flush)))) :error :pipe)
         (list (port->string (process-error p))
               (wait-exit p))))

(test* "input pipe"
       0
       (let1 p (fork-process (lambda()
                               (unless (equal? (port->string (current-input-port))
                                               "hello")
                                 (error "failed")))
                               :input :pipe)
         (with-output-to-port (process-input p)
           (lambda()
             (display "hello")
             (flush)))
         (close-output-port (process-input p))
         (wait-exit p)))

(test* "process-list"
       #t
       (let1 p (fork-process some-sleep)
         (and (not (null? (process-list)))
              (process-wait p)
              (zero? (process-exit-status p))
              (null? (process-list)))))

(test* "wait"
       0
       (let1 p (fork-process some-sleep :wait #t)
         (and (null? (process-list))
              (not (process-alive? p))
              (process-exit-status p))))

(test* "process-kill"
       SIGKILL
       (let1 p (fork-process
                (lambda()
                  (while #t
                    (some-sleep))
                  ))
         (process-kill p)
         (and (process-wait p)
              (sys-wait-signaled? (process-exit-status p))
              (sys-wait-termsig (process-exit-status p)))))

(test* "SIGTERM"
       0
       (begin
         (set-signal-handler! SIGTERM (lambda(s) (sys-exit 0)))
         (let1 p (fork-process
                  (lambda()
                    (while #t
                      (some-sleep))
                    ))
           (process-send-signal p SIGTERM)
           (wait-exit p))))

(define (sys-getmask)
  (sys-sigmask SIG_SETMASK #f))

(define (sys-sigset-inv sigset)
  (sys-sigset-delete! (sys-sigset-fill! (make <sys-sigset>)) sigset))

(test* "sigmask restore SIGTERM handler"
       SIGTERM
       (begin
         (set-signal-handler! SIGTERM #f)
         (let1 p (fork-process
                  (lambda()
                    (while #t
                      (some-sleep))
                    )
                  :sigmask '() ;; unmask all signals and restore default handlers
                  )
           (some-sleep) ;; todo
           (process-send-signal p SIGTERM)
           (and (process-wait p)
                (sys-wait-signaled? (process-exit-status p))
                (sys-wait-termsig (process-exit-status p))))))

(test* "sigmask restore signal mask"
       SIGTERM
       (begin
         (sys-sigmask SIG_BLOCK (sys-sigset SIGTERM))
         (let1 p (fork-process
                  (lambda()
                    (while #t
                      (some-sleep))
                    )
                  :sigmask '() ;; unmask all signals and restore default handlers
                  )
           (some-sleep) ;; todo
           (process-send-signal p SIGTERM)
           (and (process-wait p)
                (sys-wait-signaled? (process-exit-status p))
                (sys-wait-termsig (process-exit-status p))))))

(test* "sigmask SIGTERM"
       SIGKILL
       (let1 p (fork-process
                (lambda()
                  (while #t
                    (some-sleep))
                  )
                :sigmask (list SIGTERM) ;; mask SIGTERM
                )
         (some-sleep) ;; todo
         (process-send-signal p SIGTERM)
         (some-sleep) ;; todo
         (process-kill p)
         (and (process-wait p)
              (sys-wait-signaled? (process-exit-status p))
              (sys-wait-termsig (process-exit-status p)))))

(test* "buffering"
       "hello"
       (begin
         (with-output-to-file "test.o"
           (lambda()
             (set! (port-buffering (current-output-port)) :full)
             (display "hello")
             (fork-process flush-all-ports :wait #t)
             (flush-all-ports)))
         (port->string (open-input-file "test.o"))))

(when (version>=? (gauche-version) "0.9.1")
  ;; broken on 0.9
  (test* "standard ports"
         0
         (let1 p (fork-process
                  (lambda()
                    (let1 r
                        (and (eq? (current-output-port) (standard-output-port))
                             (eq? (current-error-port)  (standard-error-port))
                             (eq? (current-input-port)  (standard-input-port)))
                      (flush-all-ports)
                      (sys-exit (if r 0 1))))
                  :wait #t)
           (and (not (process-alive? p))
                (process-exit-status p))))

  ;; new redirects (only >=0.9.1)
  (test* "new redirect out"
         "hello"
         (port->string (process-output (fork-process
                                        (lambda()
                                          (with-output-to-port (open-output-fd-port 3)
                                            (lambda()
                                              (display "hello")
                                              (flush))))
                                        :redirects '((> 3 out))
                                        :wait #t)
                                       'out)))
  
  (test* "new redirect in"
         0
         (let1 p (fork-process
                  (lambda()
                    (sys-exit (if (with-input-from-port (open-input-fd-port 3)
                                    (cut equal? (read-line) "hello"))
                                0
                                1)))
                  :redirects '((< 3 in)))
           (with-output-to-port (process-input p 'in)
             (lambda()
               (print "hello")
               (flush)))
           (wait-exit p))))

;; (test* "fastcgi like"
;;        #t
;;        (let loop ()
;;          (set-signal-handler! 

(test "zombies?" '() process-list)

(test-end)
