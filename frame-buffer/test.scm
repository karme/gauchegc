(add-load-path ".")
(load "frame-buffer.scm")
(use ggc.skimu.frame-buffer)

(define (main args)
  (if (= (length args) 3)
      (let ((pic (load-frame-buffer-from-jpg-file (list-ref args 1))))
        (save-frame-buffer-as-jpg-file pic (list-ref args 2))
        0)
	 (errorf "Usage: ~a in.jpg out.jpg~%"
		 (list-ref args 0))))

      
