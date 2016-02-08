(use gauche.test)
(test-start "ggc.port.column")
(use ggc.port.column)
(test-module 'ggc.port.column)
(test-section "open-column-port")

(test* "first line" "(input string port)\n1\n0\n1\n1\n"
       (with-output-to-string
         (lambda ()
           (with-input-from-string  "abracadabra\n1234567890\n"
             (lambda ()
               (let ((port (open-column-port (current-input-port))))
                 (unwind-protect
                     (begin
                       (print (port-name port))
                       (print (port-current-line port))
                       (print (port-current-column port))
                       (let ((ch (read-char port)))
                         (cond ((char=? #\a ch)
                                (print (port-current-line port))
                                (print (port-current-column port)))
                               (else (error "read wrong char" ch)))))
                   (unless (port-closed? port)
                     (close-port port)))))))))

(test* "next line" "2\n0\n2\n1\n"
       (with-output-to-string
         (lambda ()
           (with-input-from-string  "abracadabra\n1234567890\n"
             (lambda ()
               (let ((port (open-column-port (current-input-port))))
                 (unwind-protect
                     (let ((l (read-line port)))
                       (cond ((string=? l "abracadabra")
                              (print (port-current-line port))
                              (print (port-current-column port)))
                             (else (error "read wrong string")))
                       (let ((ch (read-char port)))
                         (cond ((char=? #\1 ch)
                                (print (port-current-line port))
                                (print (port-current-column port)))
                               (else (error "read wrong char" ch)))))
                   (unless (port-closed? port)
                     (close-port port)))))))))

(test* "with-input-from-file/column"  "foo.txt\n1\n0\n1\n1\n2\n0\n2\n1\n"
       (with-output-to-string
         (lambda ()
           (unwind-protect
               (begin
                 (with-output-to-file "foo.txt"
                   (lambda ()
                     (display "abracadabra\n1234567890\n")))
                 (with-input-from-file/column "foo.txt"
                   (lambda ()

                     ;;  foo.txt 1 0
                     (print (port-name (current-input-port)))
                     (print (port-current-line (current-input-port)))
                     (print (port-current-column (current-input-port)))

                     ;;  1 1
                     (let ((ch (read-char)))
                       (cond ((char=? #\a ch)
                              (print (port-current-line (current-input-port)))
                              (print (port-current-column (current-input-port))))
                             (else (error "read wrong char" ch))))
                     ;;  2 0
                     (let ((l (read-line)))
                       (cond ((string=? l "bracadabra")
                              (print (port-current-line (current-input-port)))
                              (print (port-current-column (current-input-port))))
                             (else (error "read wrong string"))))

                     ;;  2 1
                     (let ((ch (read-char)))
                       (cond ((char=? #\1 ch)
                              (print (port-current-line (current-input-port)))
                              (print (port-current-column (current-input-port))))
                             (else (error "read wrong char" ch)))))))
             (sys-unlink "foo.txt")))))

(test-section "bench mark: copy /usr/share/dict/words to /dev/null")
(use gauche.time)
(use file.util)

(define (standard-file-port thunk)
  (with-output-to-file (null-device)
    (lambda ()
      (with-input-from-file "/usr/share/dict/words"
        thunk))))


(define (column-file-port thunk)
  (with-output-to-file (null-device)
    (lambda ()
      (with-input-from-file/column "/usr/share/dict/words"
        thunk))))

(define (process)
  (let lp ((ch (read-char))
           (count 1))
    (cond  ((eof-object? ch)
            (= count
               (port-current-line (current-input-port))))
           ((char=? #\newline ch)
            (write-char ch)
            (lp (read-char) (+ count 1)))
           (else
            (write-char ch)
            (lp (read-char) count)))))

(test* "standard file port" #t
  (with-error-to-port (current-output-port)
    (lambda () (time (standard-file-port process)))))

(test* "column file port" #t
  (with-error-to-port (current-output-port)
    (lambda () (time (column-file-port process)))))

(test-end :exit-on-failure #t)
