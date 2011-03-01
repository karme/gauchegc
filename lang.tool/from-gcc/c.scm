(define (main args)

  (let lp ((l (read-line)))
    (cond ((eof-object? l)
           (error "Cant find %%"))
          ((string=? l "%%") (display l) (newline))
          (else 
           (display l)
           (newline)
           (lp (read-line)))))
           
  (let lp ((c (read-char))
           (n 0))
    (if (eof-object? c)
        0
        (cond ((char=? c #\{) 
               (lp (read-char) (+ n 1)))
              ((char=? c #\})
               (lp (read-char) (- n 1)))
              ((and (char=? c #\newline) (= n 0))
               (display c)
               (if (and (char=? #\% (peek-char))
                        (display (read-char))
                        (char=? #\% (peek-char))
                        (display (read-char)))
                   #t
                   (lp (read-char) n)))
              (else
               (if (= n 0) (display c))
               (lp (read-char) n)))))

  (let lp ((l (read-line)))
    (if (eof-object? l)
        0
        (begin
          (display l)
          (newline)
          (lp (read-line))))))
           
