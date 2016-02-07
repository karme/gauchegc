;;
(use gauche.test)
(test-start "ggc.transcript-on")
(use ggc.transcript-on)
(test-module 'ggc.transcript-on)

(define test-input "\
(transcript-on \"fo.txt\")
(+ 1 2)
(print \"foo\")
(transcript-off)
")

(define expected-output "\
[fo.txt]> (+ 1 2)
3
[fo.txt]> (print \"foo\")
foo
#<undef>
[fo.txt]> (transcript-off)
")

(use file.util)
(test* "transcript-on" #t
       (begin
         (with-output-to-file (null-device)
           (lambda ()
             (with-input-from-string test-input
               (lambda ()
                 (with-module gauche.interactive (read-eval-print-loop))))))
         (string=? (file->string "fo.txt") expected-output)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF
