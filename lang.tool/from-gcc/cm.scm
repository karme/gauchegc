(define (skip c)
  (cond ((eof-object? c)
         (error "EOF inside comment"))
        ((and (char=? c #\*)
              (char=? (peek-char) #\/))
         (read-char))
        (else
         (skip (read-char)))))

(define (main args)
  (let lp ((c (read-char)))
    (cond ((eof-object? c) 0)
          ((and (char=? c #\/)
                (char=? (peek-char) #\*))
           (skip (read-char))
           (lp (read-char)))
          (else
           (display c)
           (lp (read-char))))))
