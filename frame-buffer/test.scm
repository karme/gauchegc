(use ggc.skimu.frame-buffer)

(define (copy in out)
  (print #"coping ~|in| to ~|out|")
  (let ((pic (load-frame-buffer-from-jpg-file in)))
    (save-frame-buffer-as-jpg-file pic out)
    0))

(define (main args)
  (if (= (length args) 3)
      (let* ((in  (list-ref args 1))
             (out (list-ref args 2)))
        (copy in out))
      (errorf "Usage: ~a in.jpg out.jpg~%" (list-ref args 0))))

      
