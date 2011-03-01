;;; $Id: manual.scm,v 1.1 2004/08/24 01:07:53 skimu Exp $

;;;
;;;  MIRRORING-INPUT USER'S MANUAL
;;;
;;;         written by Shigenobu Kimura <skimu@mac.com>
;;;

;;;
;;; *** INTRODUCTION ***
;;;

;;; ``Mirroring-input'' is an input-port of which each character
;;;  read by the program is copied to an associated output-port.
;;;  Input to mirror is taken from source input-port.  
;;;  An mirroring-input can be opened by calling open-mirroring-input
;;;  with a source input-port, and a destination output-port.
;;;
;;;  This is convenient in debugging, tells you how much of input-port
;;;  has been read before the error or before entering unexpected
;;;  infinite loop.
;;;

;;;
;;; *** API REFERENCE ***
;;;

;;; Function: open-mirroring-input src dest
;;;
;;;   open a mirroing-input whose source input-port is src,
;;;   and destination output-port is out.
;;;
;;;
;;; Function: call-with-mirroring-input src dest proc
;;;
;;; Function: with-input-from-port/mirroring-to-port src dest thunk
;;;
;;; Function: with-input-from-port/mirroring-to-file src file thunk
;;;
;;;   File is a string used as filename, to where 
;;;   mirrored-port copies data.
;;;
;;; Function: with-input-from-file/mirroring-to-port file dest thunk
;;;
;;;   File is a string used as filename, from where 
;;;   mirrored-port takes data.
;;;   
;;; Function: with-input-from-file/mirroring-to-file src dest thunk
;;;  
;;;   Src and dest is string wich is used for source and destination,
;;;   respectively.
;;;
;;; Function: with-input-from-string/mirroring-to-port str dest thunk
;;;  
;;;

;;;
;;; *** EXAMPLE ***
;;;
(use ggc.port.mirroring)

(define (mountain)
  (display "hello, I am mountain. I echo what I hear")
  (newline)
  (display "What do you say?: ")
  (flush)
  (let loop ((l (read-line)))
    (if (eof-object? l)
        (begin
          (newline)
          (display "No more? I miss you, bye bye.")
          (newline)
          "OK")
        (begin
          (display "I say: ")
          (write l)
          (newline)
          (display "What do you say?: ")
          (flush)
          (loop (read-line))))))

(define what-to-be-said 
"Yahoo!
Google!
")

(with-input-from-string what-to-be-said
  (lambda ()
      (with-input-from-port/mirroring-to-port
        (current-input-port)
        (current-output-port)
        mountain)))

;;; EOF
