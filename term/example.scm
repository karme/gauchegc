(use ggc.term.with-raw-mode)        

(with-raw-mode 
 (lambda ()
   (let lp ((c (read-byte)))
     (format #t "c=~s~%" c)
     (if (= c 127) ; DELETE
         (exit 0)
         (lp (read-byte))))))

