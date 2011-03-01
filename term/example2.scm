(use ggc.term.without-echo)

(let ((passwd (without-echo read-line :prompt "Password:")))
  (format #t "Your password is ~a~%" passwd)
  (format #t "Don't type your password that easy!~%"))
