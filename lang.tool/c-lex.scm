;;;
;;; C-LEX for C-PARSE
;;;

(load "c-lex-common.scm")

(define c-keywords
  '((auto        (SCSPEC    . auto))
    (break       BREAK)		
    (case        CASE)		
    (char	 (TYPESPEC  . char))
    (const	 TYPE_QUAL)
    (continue	 CONTINUE)
    (default	 DEFAULT)
    (do		 DO)
    (double	 (TYPESPEC  . double))
    (else	 ELSE)
    (enum	 ENUM)		
    (extern	 (SCSPEC   . extern))
    (float	 (TYPESPEC . float))
    (for	 FOR)
    (goto	 GOTO)
    (if		 IF)	
    (int	 (TYPESPEC  . int))	
    (long	 (TYPESPEC  . long))	
    (register	 (SCSPEC    . register))
    (return	 RETURN)
    (short	 (TYPESPEC  . short))
    (signed	 (TYPE_QUAL . signed))
    (sizeof	 SIZEOF)
    (static	 STATIC)    ; not SCSPEC
    (struct	 STRUCT)
    (switch	 SWITCH)
    (typedef	 (SCSPEC    . typedef))
    (union	 UNION)
    (unsigned	 (TYPE_QUAL . unsigned))
    (void	 (TYPESPEC  . void))
    (volatile	 (TYPE_QUAL . volatile))
    (while	 WHILE)
    (__builtin_va_list (EXTENSION . __builtin_va_list))
    ))

(define c-operators
  '((>>=         (ASSIGN . >>=))
    (<<=         (ASSIGN . <<=))
    (+=          (ASSIGN . +=))
    (-=          (ASSIGN . -=))
    (*=          (ASSIGN . *=))
    (/=          (ASSIGN . /=))
    (%=          (ASSIGN . %=))
    (&=          (ASSIGN . &=))
    (^=          (ASSIGN . ^=))
;;  (|=          (ASSIGN . |=))  ; scheme
    (>>          RSHIFT)
    (<<          LSHIFT)
    (++          PLUSPLUS)
    (--          MINUSMINUS)
    (->          PTR_EXTENT)
    (&&          ANDAND)
;;  (||          OROR)           ; scheme
    (<=          (ARITHCOMPARE . <=))
    (>=          (ARITHCOMPARE . >=))
    (<           (ARITHCOMPARE . <))
    (>           (ARITHCOMPARE . >))
    (==          (EQCOMPARE . ==))
    (!=          (EQCOMPARE . !=))
    (*           *)
    (/           /)
    (+           +)
    (-           -)
    (=           =)
    (%           %)
    (&           &)
    (^           ^)
    (?           ?)
    ))

(define typedefed-identifiers '())

;;;
;;;
;;;
(define (c-lex)
  (skip-spaces)
  (let loop ((c (read-char)))
    (cond
     ((eof-object? c)  '*eoi*)
     ((char=? c #\newline) 
      (inc! lineno)
      (skip-spaces)
      (loop (read-char)))
     ((char=? c #\#)                    ; XXX Check beginning of line?
      (do-sharp-command)
      (loop (read-char)))

     ((char=? c #\0)
      (cond 
       ((char-ci=? (peek-char) #\x)
        (begin (read-char)
               (cons 'CONSTANT (read-hexadecimal '(#\x #\0)))))
       (else
        (cons 'CONSTANT (read-octal-or-flonum (list c))))))

     ((char-numeric? c)
      (cons 'CONSTANT (read-decimal (list c))))

     ((and (char=? c #\.) (char-numeric? (peek-char)))
      (cons 'CONSTANT (read-flonum (list c) #[0-9] 10 #[Ee])))

     ((char=? c #\.)  
      (if (char=? (peek-char) #\.)
          (begin
            (read-char)
            (if (char=? (peek-char) #\.)
                (begin
                  (read-char)
                  'ELLIPSIS)
                (error "syntax error ..")))
          'DOT))

     ((and (char=? c #\L) (char=? (peek-char) #\"))
      (read-char)                       ; L
      (cons 'STRING   (read-string-literal)))
     ((and (char=? c #\L) (char=? (peek-char) #\'))
      (read-char)                       ; L
      (cons 'CONSTANT (list 'wchar (read-character-constant))))
     ((char=? c #\")
      (cons 'STRING   (read-string-literal)))
     ((char=? c #\')
      (cons 'CONSTANT (list 'int (read-character-constant))))

     ((char-set-contains? initial-identifier-charset c)
      (read-identifier (list c)))

     ((char-set-contains? operator-charset c)
      (read-operator c))

     ((char=? c #\,)   'COMMA)
     ((char=? c #\:)   'COLON)
     ((char=? c #\;)   'SEMICOLON)
     ((char=? c #\()   'LPAREN)
     ((char=? c #\))   'RPAREN)
     ((char=? c #\{)   'LCBRA)
     ((char=? c #\})   'RCBRA)
     ((char=? c #\[)   'LSBRA)
     ((char=? c #\])   'RSBRA)

     ;; special case for c-operator due to Scheme
     ((char=? c #\|)   (or (follow #\| 'OROR          #f)
                           (follow #\= '(ASSIGN . OR) #f)
                           'OR))
     (else
      (error "waring: illegal character: " c)
      (skip-spaces)
      (loop (read-char))))))
;;;;;;
;;;;;; END OF LEXER PART

(use gauche.sequence)
(use ggc.port.mirroring)

(define (ppvec v)
  (define (ff v n)
    (let ((sp (make-string n #\|)))
      (define (wri x) (display sp) (write x) (newline))
      (for-each (lambda (x)
                  (if (vector? x)
                      (ff x (+ n 1))
                      (wri x)))
                v)))
  (newline)
  (ff v 1))

(if #f

    (define (main args)
      (with-input-from-port/mirroring-to-port
       (current-input-port)
       (current-output-port)
       (lambda ()
         (do ((r (c-lex) (c-lex)))
             ((eq? r '*eoi*) 0)
           (display #\") (write r) (display #\")
           (newline)))))

    (define (main args)
      (load "c-parse.yy.scm")
      (with-input-from-port/mirroring-to-port
       (current-input-port)
       (current-output-port)
       (lambda () (c-parse c-lex error)))
      (display "Typedefed : ")
      (display typedefed-identifiers)
      (newline)
      0)
    )




;; EOF
