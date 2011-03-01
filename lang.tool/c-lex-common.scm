;;;
;;; Common parts for C-LEX and C-SCAN
;;;
;;; Followings must be processed in prior to this lexer.
;;; (most likely in c-preprocessor)
;;;
;;;  0) Comment (both /* */ and //)
;;;  1) Merger of lines by backslash(\)  (K&R A12.2)
;;;  2) Trigraph. (CARM 2.1.4, K&R A12.1)
;;;  3) ## operator. 
;;;  4) alternate token spelling (CARM 2.4) for #, ##.
;;;

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

(define typedefed-identifiers '())

;;;
;;;
;;;
(define (get-keyword-val symbol) (assq symbol c-keywords))

(define (get-operator-val symbol) (assq symbol c-operators))

(define (typedefed? symbol) (memq symbol typedefed-identifiers))

(define in-typedef #f)

(define (install-type name type) 

  (define (type-compatible? T1 T2) (equal? T1 T2))
  
  (format #t "### installing type ~a~%" symbol)
  (let ((T (assq name typedefed-indentifiers)))
    (if (and T (not (type-compatible? (cdr T) type)))
        (errorf "### typedef ~a ~%" type)))
  (set! typedefed-identifiers
        (cons (cons name type) typedefed-identifiers)))

;;;
;;;
;;;
;; gcc of GCC allows dollar sign
;;(define initial-identifier-charset #[A-Za-z_])
;;(define identifier-charset         #[A-Za-z_0-9])  
(define initial-identifier-charset #[A-Za-z_$])
(define identifier-charset         #[A-Za-z_$0-9])

(define operator-charset           #[*~!+\-/\^&%=?<>])
(define special-operator-charset   #[|.]) ; cant be in scheme symbol

(define scheme-token-charset 
  (char-set    #\.                 ; DOT
	       #\:                 ; COLON
	       #\(                 ; LPAREN
	       #\)                 ; RPAREN
	       #\{                 ; LCBRA
	       #\}                 ; RCBRA
	       #\[                 ; LSBRA
	       #\]                 ; RSBRA
	       #\|                 ; OR
	       ))

(define (char-digit? c)         (char-set-contains? #[0-9] c))
(define (char-nonzero-digit? c) (char-set-contains? #[1-9] c))
(define (char-hex-digit? c)     (char-set-contains? #[0-9A-Fa-f] c))
(define (char-octal-digit? c)   (char-set-contains? #[0-7] c))

(define (skip-spaces)
  (let loop ((c (peek-char)))
    (if (and (not (eof-object? c))
	     (or (char=? c #\space)
		 (char=? c #\tab)
		 (char=? c #\x0b)     ; VT  
		 (char=? c #\page)))
        (begin
          (read-char)
          (loop (peek-char))))))

(define (skip-line)
  (let loop ((c (peek-char)))
    (if (and (not (eof-object? c))
             (not (char=? c #\newline)))
        (begin
          (read-char)
          (loop (peek-char))))))

(define (l->symbol l)
  (string->symbol (apply string (reverse l))))

(define (read-identifier l)
  (let ((c (peek-char)))
    (if (char-set-contains? identifier-charset c)
        (begin
          (read-char)
          (read-identifier (cons c l)))
        (let* ((s (l->symbol l))
               (k (get-keyword-val s)))
          (if k
              (cadr k)
              (if (typedefed? s)
                  (cons 'TYPENAME s)
                  (cons 'IDENTIFIER s)))))))

(define (read-operator c)
  (define (l->opval l)
    (get-operator-val (l->symbol l)))
  (let lp ((cand (l->opval (list c)))
	   (l (list c)))
    (let* ((c (peek-char))
	   (k (l->opval (cons c l))))
      (if k
	  (begin
	    (read-char)
	    (lp k (cons c l)))
	  (cadr cand)))))

;;;
;;;
;;;
(define (hexchar->integer c)
  (cond ((char=? c #\0) 0)
        ((char=? c #\1) 1)
        ((char=? c #\2) 2)
        ((char=? c #\3) 3)
        ((char=? c #\4) 4)
        ((char=? c #\5) 5)
        ((char=? c #\6) 6)
        ((char=? c #\7) 7)
        ((char=? c #\8) 8)
        ((char=? c #\9) 9)
        ((char-ci=? c #\a) 10)
        ((char-ci=? c #\b) 11)
        ((char-ci=? c #\c) 12)
        ((char-ci=? c #\d) 13)
        ((char-ci=? c #\e) 14)
        ((char-ci=? c #\f) 15)
        (else
         (error "can not be, there must be a bug in lexer"))))

(define (backslash c)

  (define (readn c n v)
    (if (and (> n 0) (char-octal-digit? c))
        (begin
          (read-char)
          (readn (peek-char) 
                 (- n 1) 
                 (+ (* v 8) (hexchar->integer c))))
        (integer->char v)))

  (define (readx c)
    (let lp ((c c) 
             (v 0))
      (if  (char-hex-digit? c)
           (begin
             (read-char)
             (lp (peek-char)
                 (+ (* v 16) (hexchar->integer c))))
           (integer->char v))))

  (define (readu c)

    (define (range-check v) 
      ;; CARM: Sec 2.9, p41
      (or (= v (string->number "0024" 16))
          (= v (string->number "0040" 16))
          (= v (string->number "0060" 16))
          (not (< v (string->number "00A0" 16)))
          (not (and (<= (string->number "D800" 16) v)
                    (<= v (string->number "DFFF" 16))))))
    (receive (n v)
        (let lp ((c c) 
                 (n 1) 
                 (v 0))
          (if (char-hex-digit? c)
              (begin
                (read-char)
                (lp (peek-char) 
                    (+ n 1)
                    (+ (* v 16) (hexchar->integer c))))
              (values n v)))
      (if (not (or (= n 4)
                   (= n 8)))
          (error "Universal character name must be 4 or 8 hex-digit"))
      (range-check v)
      (ucs-char v)))

  (if (char=? c #\\)
      (let ((c (read-char)))
        (cond                
         ((char=? c #\a) #\x07)         ; BEL  07  ^G
         ((char=? c #\b) #\x08)         ; BS   08  ^H
         ((char=? c #\t) #\tab)         ; HT   09  ^I
         ((char=? c #\n) #\newline)	; NL   0A  ^J
         ((char=? c #\v) #\x0b)         ; VT   0B  ^K
         ((char=? c #\f) #\page)	; NP   0C  ^L
         ((char=? c #\r) #\return)	; CR   0D  ^M
         ((char=? c #\x)   ; not char-ci=?,  cf. CARM p. 35
          (if (char-hex-digit? (peek-char))
              (readx (peek-char))
              (error "\\x must be followed by hex-digit" (peek-char))))
         ((char-ci=? c #\u)
          (if (char-hex-digit? (peek-char))
              (readu (peek-char))
              (error "\\u must be followed by hex-digit" (peek-char))))
         ((char-octal-digit? c)
          (readn (peek-char) 3 (hexchar->integer c)))
         ((char-set-contains? #[?\'\"\\] c) c) ; first two backslash 
                                               ; are placed to fool emacs
         ((char-set-contains? #[a-z] c)
          (error "Warning: unknown lower case espace character is used" c))
         ((char-set-contains? #[A-Z] c)
          (error "No upper case espace character is defined" c))
         (else
          (read-char))))
      c))

(define (read-string-literal)
  (let lp ((c (read-char))
           (s '()))
    (if (eof-object? c)
        (error "missing double quote")
        (if (char=? c #\")
            (apply string (reverse s))
            (let ((cc (backslash c)))
              (lp (read-char) (cons cc s)))))))

(define (read-character-constant)
  (let lp ((c (read-char))
           (s 0))
    (if (eof-object? c)
        (error "missing quote")
        (if (char=? c  #\')
            (number->string s)
            (let ((cc (backslash c)))
              ;; Meaning of Multicharacter constant is implementation
              ;; dependent.  Here we implement a convention with left-to-right 
              ;; packing, which is  described in CARM pp. 31--32.
              (lp (read-char) 
                  (+ (* 256 s) 
                     (char->integer cc))))))))
;;;
;;; read-hexadecimal, octal.
;;;
(define (read-octal-or-flonum l)
  (define (rl->n l)
    (for-each (lambda (c) 
                (if (not (char-set-contains? #[0-7] c))
                    (error "invalid char in octal" c)))
              l)
    (apply string (reverse l)))

  (let ((c (peek-char)))
    (cond 
     ((char-set-contains? #[0-9] c)
      (read-char)
      (read-octal-or-flonum (cons c l)))
     ((char=? #\. c)
      (read-char)
      (read-flonum (cons c l) #[0-9] 10 #[Ee]))
     ((char-set-contains? #[Ee] c) 
      (read-char)
      (read-expnum (cons c l) 10))
     ((char-set-contains? #[ULul] c)
      (read-char)
      (list (integer-suffix c)
            (rl->n l)))
     (else
      (list 'int (rl->n l))))))
  
;;;
;;; read decimal integer or floating point
;;;
(define (read-number-constant l ics radix ecs)

  (define (rl->n l)
    (apply string (reverse l)))

  (let ((c (peek-char)))
    (cond 
     ((char-set-contains? ics c)
      (read-char)
      (read-number-constant (cons c l) ics radix ecs))
     ((char=? c #\.)
      (read-char)
      (read-flonum (cons c l) ics radix ecs))
     ((char-set-contains? ecs c) 
      (read-char)
      (read-expnum (cons c l) radix))
     ((char-set-contains? #[ULul] c)
      (read-char)
      (list (integer-suffix c)
            (rl->n l)))
     (else
      (list 'int (rl->n l))))))

(define (read-decimal l)
  (read-number-constant l #[0-9] 10 #[Ee]))
(define (read-hexadecimal l) 
  (read-number-constant l #[0-9A-Fa-f] 16 #[Pp]))

(define (integer-suffix c)
  ;; ugly...
  (cond 
   ((char-ci=? c #\u)
    (let ((c (peek-char)))
      (cond ((char=? c #\l)
             (read-char)
             (let ((c (peek-char)))
               (if (char=? c #\l)
                   (begin
                     (read-char)
                     'unsigned-long-long)
                   'unsigned-long)))
            ((char=? c #\L)
             (read-char)
             (let ((c (peek-char)))
               (if (char=? c #\L)
                   (begin
                     (read-char)
                     'unsigned-long-long)
                   'unsigned-long)))
            (else
             'unsigned-int))))

   ((char=? c #\l)
    (let ((c (peek-char)))
      (cond ((char=? c #\l)
             (begin
               (read-char)
               (let ((c (peek-char)))
                 (if (char-ci=? c #\u)
                     (begin (read-char)
                            'unsigned-long-long)
                     'long-long))))
            ((char-ci=? c #\u)
             (read-char)
             'unsinged-long)
            (else  'long))))

   ((char=? c #\L)
    (let ((c (peek-char)))
      (cond ((char=? c #\L)
             (begin
               (read-char)
               (let ((c (peek-char)))
                 (if (char-ci=? c #\u)
                     (begin (read-char)
                            'unsigned-long-long)
                     'long-long))))
            ((char-ci=? c #\u)
             (read-char)
             'unsinged-long)
            (else  'long))))
   (else
    (error "there is a bug in lexer"))))

(define (read-flonum l ics radix ecs)
  ;;(define (rl->n l)
  ;;  (string->number (apply string (reverse l))))
  (define (rl->n l) (apply string (reverse l)))

  (define (error-if-hex)
    (if (= radix 16)
        (error "hexadecimal floating constants require an exponent")))

  (let ((c (peek-char)))
    (cond
     ((char-set-contains? ics c)
      (read-char)
      (read-flonum (cons c l) ics radix ecs))
     ((char-set-contains? ecs c) 
      (read-char)
      (read-expnum (cons c l) radix))
     ((char-ci=? c #\f)
      (read-char)
      (error-if-hex)
      (list 'float (rl->n l)))
     ((char-ci=? c #\l)
      (read-char)
      (error-if-hex)
      (list 'long-double (rl->n l)))
     (else
      (error-if-hex)
      (list 'double (rl->n l))))))

;;;
;;;
;;;
(define (read-expnum l radix)

  (define (rl->n l) (apply string (reverse l)))

  (define (exp1 c l)
    (cond
     ((char-numeric? c)
      (read-char)
      (exp1 (peek-char) (cons c l)))
     ((char-ci=? c #\f)
      (read-char)
      (list 'float (rl->n l)))
     ((char-ci=? c #\l)
      (read-char)
      (list 'long-double (rl->n l)))
     (else
      (list 'double (rl->n l)))))
    
  (if (char-set-contains? #[0-9\-\+] (peek-char))
      (let ((c (read-char)))
        (exp1 (peek-char) (cons c l)))
      (error "malformed floating point expression")))

;;;
;;;
;;;
(define (follow expect ifyes ifno)
  (let ((c (peek-char)))
    (if (char=? c expect)
        (begin (read-char)
               ifyes)
        ifno)))

;;;
;;;
;;;

(define lineno 1)

(define sc-ignore         skip-line)
(define sc-do-sharp-space skip-line)

(define sharp-commands
  `(pragma  ,sc-ignore)
  )

(define (do-sharp-command)
  (if (char=? (peek-char) #\space)
      (begin
	(read-char)
	(sc-do-sharp-space))
      (let lp ((c (peek-char))
	       (l '()))
	(cond ((char-alphabetic? c)
	       (read-char)
	       (lp (peek-char) (cons c l)))
	      (else
	       (let ((cmd (assq (l->symbol l) sharp-commands)))
		 (if cmd 
		     ((cadr cmd))
		     (sc-ignore))))))))
;;;
;;;
;;;
(define (c-lexer)
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
                           (follow #\= 'OR_ASSIGN #f)
                           'OR))
     (else
      (error "waring: illegal character: " c)
      (skip-spaces)
      (loop (read-char))))))
;;;;;;
;;;;;; END OF LEXER PART

;;;
;;;
;;;
(define (main args)

  (define *eof* (with-input-from-string "" read-char))

  (define passed 0)
  (define failed 0)

  (define (run-lexer)
    (guard (e ((is-a? e <error>)
               (cons 'ERROR (ref e 'message)))
              (else
               (error "Unexpected exception")))
       (port-map (lambda (x) x) 
                 (lambda ()
                   (let ((x (c-lexer)))
                     (if (eq? x '*eoi*) 
                         *eof*
                         x))))))

  (define (test in expect)
    (let ((r  (with-input-from-string in run-lexer)))
      (format #t "~20a -> ~40,,,,40:s : ~a~%"
              in
              r
              (if (equal? r expect) 
                  (begin
                    (inc! passed)
                    'ok )
                  (begin
                    (inc! failed)
                    (display "")
                    'FAIL!!)))))
  
  (define (report)
    (format #t "===================================~%")
    (format #t "~a failed, out of ~a tests~%"
            failed (+ failed passed))
    (format #t "===================================~%")
    )

  ;; 
  ;;  TESTS START HERE
  ;;

  ;;
  ;; CARM 2.3 TOKENS
  ;;
  (test "forwhile;" '((IDENTIFIER . forwhile) SEMICOLON))
  (test "b>x;"   '((IDENTIFIER . b) >    (IDENTIFIER . x) SEMICOLON))
  (test "b->x;"  '((IDENTIFIER . b) PTR_OP   (IDENTIFIER . x) SEMICOLON))
  (test "b--x;"  '((IDENTIFIER . b) DEC_OP   (IDENTIFIER . x) SEMICOLON))
  (test "b---x;" '((IDENTIFIER . b) DEC_OP - (IDENTIFIER . x) SEMICOLON))
  ;;
  ;; integer constants
  ;;
  (test "1234;"    '((CONSTANT int "1234") SEMICOLON))
  (test "012;"     '((CONSTANT int "012")  SEMICOLON))
  (test "0x12;"    '((CONSTANT int "0x12") SEMICOLON))
  ;;
  ;; character constants
  ;;
  (test "'a';"     '((CONSTANT int "97")   SEMICOLON))
  (test "'A';"     '((CONSTANT int "65")   SEMICOLON))
  (test "' ';"     '((CONSTANT int "32")   SEMICOLON))
  (test "'?';"     '((CONSTANT int "63")   SEMICOLON))
  (test "'\\r';"   '((CONSTANT int "13")   SEMICOLON))
  (test "'\\0';"   '((CONSTANT int "0")    SEMICOLON))
  (test "'\"';"    '((CONSTANT int "34")   SEMICOLON))
  ;; "255" or "-1", depending on the size of char
  (test "'\\377';" '((CONSTANT int "255")  SEMICOLON))
  (test "'%';"     '((CONSTANT int "37")   SEMICOLON))
  (test "'\\23';"  '((CONSTANT int "19")   SEMICOLON))
  (test "'8';"     '((CONSTANT int "56")   SEMICOLON))
  (test "'\\\\';"  '((CONSTANT int "92")   SEMICOLON))
  ;; (string->number "41424344" 16) -> 1094861636
  (test "'ABCD';"  '((CONSTANT int "1094861636") SEMICOLON))
  ;;
  ;; floating point constants
  ;;
  (test "0.;"       '((CONSTANT double "0.")       SEMICOLON))
  (test "3e1;"      '((CONSTANT double "3e1")      SEMICOLON))
  (test "3.14159;"  '((CONSTANT double "3.14159")  SEMICOLON))
  (test ".0;"       '((CONSTANT double ".0")       SEMICOLON))
  (test "1.0E-3;"   '((CONSTANT double "1.0E-3")   SEMICOLON))
  (test "1e-3;"     '((CONSTANT double "1e-3")     SEMICOLON))
  (test "1.0;"      '((CONSTANT double "1.0")      SEMICOLON))
  (test "0.00034;"  '((CONSTANT double "0.00034")  SEMICOLON))
  (test "2e+9;"     '((CONSTANT double "2e+9")     SEMICOLON))
  ;; STDC floating point
  (test "1.0f;"     '((CONSTANT float "1.0")          SEMICOLON))
  (test "1.0e67L;"  '((CONSTANT long-double "1.0e67") SEMICOLON))
  (test "0E1L;"     '((CONSTANT long-double "0E1")   SEMICOLON))
  (test "0x1.0p1;"  '((CONSTANT double "0x1.0p1") SEMICOLON))
  (test "0x1.0;"    '(ERROR . "hexadecimal floating constants require an exponent"))
  ;;
  ;; String constants
  ;;
  (test "\"\";" '((STRING . "") SEMICOLON))
  (test "\"\\\"\";" '((STRING . "\"") SEMICOLON))
  (test "\"Copyright 2000 \\\nTexas Instruments. \"" 
   '((STRING . "Copyright 2000 Texas Instruments. ")))
  (test "\"Comments begin with '/*'.\\n\""
   '((STRING . "Comments begin with '/*'.\n")))
  (test "X++Y;" 
   '((IDENTIFIER . X) INC_OP (IDENTIFIER . Y) SEMICOLON)
   )
  (test "-12ul;"
   '(- (CONSTANT unsigned-long "12") SEMICOLON) 
   )
  (test "1.37E+6L;"
   '((CONSTANT long-double "1.37E+6") SEMICOLON)  
   )
  (test "\"String \"\"FOO\"\"\""
   '((STRING . "String ") (STRING . "FOO") (STRING . ""))
   )
  (test "\"Strinng+\\\"FOO\\\"\""
   '((STRING . "Strinng+\"FOO\""))
   )
  (test "x**2;"
   '((IDENTIFIER . x) * * (CONSTANT int "2") SEMICOLON) 
   )
  ;;Trigraphs are supposed to be processed in prior to lexer
  ;;(test "\"X??/\"" '())
  ;;Dollar sign canot be a part of identifier
  ;;(test "B$C;" '())
  (test "A*=B;"
   '((IDENTIFIER . A) MUL_ASSIGN (IDENTIFIER . B) SEMICOLON)
   )
  ;; ## operator is processed by cpp, lexer does not expect to see it.
  ;;(test "while##DO;" '())
  (report)
  )

;; EOF
