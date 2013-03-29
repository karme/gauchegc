#| -*- mode: scheme; coding: utf-8; -*- |#

;; todo: maybe just run stty on the device?

(define-module terminal-size
  (use c-wrapper)
  (export
   get-terminal-size
   set-terminal-size!))

(select-module terminal-size)
(c-load '("sys/ioctl.h") :module #f)

(define (get-terminal-size fd)
  (let ((s (make <c-struct:winsize>)))
    (when (not (zero? (ioctl fd TIOCGWINSZ (ptr s))))
      (error "ioctl failed"))
    (list (ref s 'ws_col) (ref s 'ws_row))))

(define (set-terminal-size! fd cols rows)
  (let ((s (make <c-struct:winsize>)))
    (set! (ref s 'ws_col) cols)
    (set! (ref s 'ws_row) rows)
    (when (not (zero? (ioctl fd TIOCSWINSZ (ptr s))))
      (error "ioctl failed"))))
