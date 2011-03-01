;;;
;;; C-SCAN for C-GRAM
;;;

(load "c-lex-common.scm")

(define c-keywords
  '((auto        AUTO)
    (break       BREAK)		
    (case        CASE)		
    (char	 CHAR)		
    (const	 CONST)		
    (continue	 CONTINUE)	
    (default	 DEFAULT)	
    (do		 DO)		
    (double	 DOUBLE)	
    (else	 ELSE)		
    (enum	 ENUM)		
    (extern	 EXTERN)	
    (float	 FLOAT)		
    (for	 FOR)		
    (goto	 GOTO)		
    (if		 IF)		
    (int	 INT)		
    (long	 LONG)		
    (register	 REGISTER)	
    (return	 RETURN)	
    (short	 SHORT)		
    (signed	 SIGNED)	
    (sizeof	 SIZEOF)	
    (static	 STATIC)	
    (struct	 STRUCT)	
    (switch	 SWITCH)	
    (typedef	 TYPEDEF)	
    (union	 UNION)		
    (unsigned	 UNSIGNED)	
    (void	 VOID)		
    (volatile	 VOLATILE)	
    (while	 WHILE)
    (__builtin_va_list ELLIPSIS)
    ))

(define c-operators
  '((>>=         RIGHT_ASSIGN)
    (<<=         LEFT_ASSIGN)
    (+=          ADD_ASSIGN)
    (-=          SUB_ASSIGN)
    (*=          MUL_ASSIGN)
    (/=          DIV_ASSIGN)
    (%=          MOD_ASSIGN)
    (&=          AND_ASSIGN)
    (^=          XOR_ASSIGN)
    ;;(|=          OR_ASSIGN)
    (>>          RIGHT_OP)
    (<<          LEFT_OF)
    (>           >)
    (<           <)
    (++          INC_OP)
    (--          DEC_OP)
    (->          PTR_OP)
    (&&          AND_OP)
    ;;(||          OR_OP)
    (<=          LE_OP)
    (>=          GE_OP)
    (==          EQ_OP)
    (!=          NE_OP)
    (*           *)
    (/           /)
    (+           +)
    (-           -)
    (%           %)
    (&           &)
    (^           ^)
    (=           =)
    (?           ?)
    ))

;;;
;;; Changes from c-lex.scm:
;;;
;;;   STRING        -> STRING_LITERAL
;;;   OROR          -> OR_OP
;;;   (ASSIGN . OR) -> OR_ASSIGN
;;;
(define (c-scan)
  (skip-spaces)
  (let loop ((c (read-char)))
    (cond
     ((eof-object? c)  '*eoi*)
     ((char=? c #\newline) 
      (inc! lineno)
      (skip-spaces)
      (loop (read-char)))
     ((char=? c #\#)                    ; XXX Check beginning of line?
      (read-char)
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
     ((char=? c #\|)   (or (follow #\| 'OR_OP     #f)   ; ||
                           (follow #\= 'OR_ASSIGN #f)   ; |=
                           'OR))                        ; |
     (else
      (errorp "waring: illegal character: " c)
      (skip-spaces)
      (loop (read-char))))))
;;;
;;;
;;;

(use ggc.port.mirroring)
(use gauche.sequence)

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

(define (main args)
  (load "c-gram.yy.scm")
  (with-input-from-port/mirroring-to-port
   (current-input-port)
   (current-output-port)
   (lambda () (c-gram c-scan error)))
  0)
