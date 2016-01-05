(use gauche.test)
(test-start "ggc.port.mirroring")
(use ggc.port.mirroring)
(test-module 'ggc.port.mirroring)

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

(test* "check if port is closed on error" #t
       (let ((port #f))
         (guard (e (else (and port (port-closed? port))))
           (with-input-from-string/mirroring-to-port
            "abracadabra" (current-output-port)
            (lambda ()
              (set! port (current-input-port))
              (read-char)
              (error "for test"))))))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

                     
                  
