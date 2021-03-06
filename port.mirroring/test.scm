(use gauche.test)
(test-start "ggc.port.mirroring")

(use ggc.port.mirroring)
(test-module 'ggc.port.mirroring)

(test-section "mirroring input")

(test* "read one char" "a"
       (with-output-to-string
         (lambda ()
           (with-input-from-string/mirroring-to-port
            "abracadabra" (current-output-port)
            (lambda ()
              (read-char))))))

(test* "read two char" "ab"
       (with-output-to-string
         (lambda ()
           (with-input-from-string/mirroring-to-port
               "abracadabra" (current-output-port)
             (lambda ()
               (read-char)
               (read-char)
               )))))

(test* "check if port gets closed on error" #t
       (let ((port #f))
         (guard (e (else (and port (port-closed? port))))
           (with-input-from-string/mirroring-to-file
               "abracadabra" "fo.txt"
             (lambda ()
               (set! port (current-input-port))
               (read-char)
               (error "for test"))))))

(test* "port-name returns #f" "#f\n"
       (with-output-to-string
         (lambda ()
           (with-input-from-string/mirroring-to-port
               "abracadabra" (current-output-port)
             (lambda ()
               (print (port-name (current-input-port))))))))

(test* "port-current-line works fine" "\n\n3\n"
       (with-output-to-string
         (lambda ()
           (with-input-from-string/mirroring-to-port
               "\n\nabracadabra" (current-output-port)
             (lambda ()
               (read-char)              ; outs #\nl
               (read-char)              ; outs #\nl
               (print (port-current-line (current-input-port))) ; outs #\3 #\nl
               )))))

(test-section "mirroring output")

(test* "mirroring output" "foo\n"
       (with-output-to-string
         (lambda ()
           (with-output-to-port/mirroring-to-file
               (current-output-port) "fo.txt"
             (lambda ()
               (print "foo"))))))

(use gauche.version)
(when (version>? (gauche-version) "0.9.5_pre0")
  (test-section "gpsh")
  ;;
  ;; test-script was introduced, Feb 4 19:06:44 2016 -1000
  ;;
  (test-script "./gpsh"))


;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
