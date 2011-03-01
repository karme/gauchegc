(use lalr)

(define c-decl
  (lalr-parser
   (expect: 1)  ; IF-ELSE
   ;;   (output:    c-gram "c-gram.yy.scm")
   ;;   (out-table: "c-gram.out")
   (ID
    ;; For scheme
    SEMICOLON ;
    ;; COMMA=,  LCBRA={  RCBRA=} LSBRA=[  RSBRA=]
    ;; LPAREN=( RPAREN=) OR=| DOT=.
    COMMA LCBRA RCBRA LSBRA RSBRA 
    LPAREN RPAREN OR DOT COLON

    ~ ! + - * / ^ & % = ? < >

    IDENTIFIER CONSTANT STRING SIZEOF
    PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
    AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
    SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
    XOR_ASSIGN OR_ASSIGN TYPENAME

    TYPEDEF EXTERN STATIC AUTO REGISTER
    CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
    STRUCT UNION ENUM ELLIPSIS RANGE

    CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
    )

   (program
    ()
    (file)
    )

   (file
    (external_definition)
    (file external_definition)
    )

   (external_definition
    (function_definition)   : (begin (newline) (pppp $1) (newline))
    (declaration)           : (begin (newline) (pppp $1) (newline))
    )

   (function_definition
    (declarator function_body) 
    : (list 'function_definition 'NIL $1 $2)
    (declaration_specifiers declarator function_body) 
    : (list 'function_definition  $1 $2 $3)
    )

   (function_body
    (compound_statement)
    : (function_body 'NIL $1)
    (declaration_list compound_statement)
    : (function_body  $1  $2)
    )

   (declaration
    (declaration_specifiers SEMICOLON) 
    : (list 'declaration $1 'NIL)
    (declaration_specifiers init_declarator_list SEMICOLON) 
    : (list 'declaration $1 $2)
    )

   (declaration_specifiers
    (storage_class_specifier)
    : (list 'declaration_specifiers $1 'NIL 'NIL)
    (storage_class_specifier declaration_specifiers)
    : (list 'declaration_specifiers $1 'NIL $2)
    (type_specifier)
    : (list 'declaration_specifiers 'NIL $1 'NIL)
    (type_specifier declaration_specifiers)
    : (list 'declaration_specifiers 'NIL $1 $2)
    )

   (init_declarator_list
    (init_declarator) : (list $1)
    (init_declarator_list COMMA init_declarator) : (append $1 (list $3))
    )

   (init_declarator
    (declarator)               : (list 'init_declarator $1 'NIL)
    (declarator = initializer) : (list 'init_declarator $1 $3)
    )

   (storage_class_specifier
    (TYPEDEF)  : 'TYPEDEF
    (EXTERN)   : 'EXTERN
    (STATIC)   : 'STATIC
    (AUTO)     : 'AUTO
    (REGISTER) : 'REGISTER
    )

   (type_specifier
    (CHAR)     : 'CHAR
    (SHORT)    : 'SHORT
    (INT)      : 'INT
    (LONG)     : 'LONG
    (SIGNED)   : 'SIGNED
    (UNSIGNED) : 'UNSIGNED
    (FLOAT)    : 'FLOAT
    (DOUBLE)   : 'DOUBlE
    (CONST)    : 'CONST
    (VOLATILE) : 'VOLATILE
    (VOID)     : 'VOID
    (struct_or_union_specifier) : $1
    (enum_specifier)            : $1
    (TYPENAME) : $1
    )

   (primary_expr 
    (identifier)         : (list 'VAREF $1)
    (CONSTANT)           : (list 'CONST $1)
    (STRING)             : (list 'STRING $1)
    (LPAREN expr RPAREN) : (list $2)
    )

   (postfix_expr 
    (primary_expr)
    : $1
    (postfix_expr LSBRA expr RSBRA)
    : (list 'ARRAY_REF $1 $3)
    (postfix_expr LPAREN RPAREN)
    : (list 'FUNCALL $1 'NIL)
    (postfix_expr LPAREN argument_expr_list RPAREN)
    : (list 'FUNCALL $1 $3)
    (postfix_expr DOT identifier)
    : (list 'STRUCT_REF $1 $3)
    (postfix_expr PTR_OP identifier)
    : (list 'STRUCT_PTR_REF $1 $3)
    (postfix_expr INC_OP)
    : (list 'INC $1)
    (postfix_expr DEC_OP)
    : (list 'DEC $1)
    )

   (argument_expr_list
    (assignment_expr)                          
    : (list $1)
    (argument_expr_list COMMA assignment_expr)
    : (append $1 (list $3))
    )

   (unary_expr 
    (postfix_expr)             : $1
    (INC_OP unary_expr)        : (list 'INC $2)
    (DEC_OP unary_expr)        : (list 'DEC $2)
    (unary_operator cast_expr) : (list $1 $2)
    (SIZEOF unary_expr)        : (list 'SIZEOF $2)
    (SIZEOF LPAREN type_name RPAREN) : (list 'SIZEOF $3)
    )

   (unary_operator 
    (&) : '&
    (*) : '*
    (+) : '+
    (-) : '-
    (~) : '~
    (!) : '!
    )

   (cast_expr
    (unary_expr) : $1
    (LPAREN type_name RPAREN cast_expr) : (list 'CAST $4 $2)
    )

   (multiplicative_expr
    (cast_expr) : $1
    (multiplicative_expr * cast_expr) : (list '* $1 $3)
    (multiplicative_expr / cast_expr) : (list '/ $1 $3)
    (multiplicative_expr % cast_expr) : (list '% $1 $3)
    )

   (additive_expr
    (multiplicative_expr) : $1
    (additive_expr + multiplicative_expr) : (list '* $1 $3)
    (additive_expr - multiplicative_expr) : (list '- $1 $3)
    )

   (shift_expr
    (additive_expr) : $1
    (shift_expr LEFT_OP additive_expr)    : (list 'LSHIFT $1 $3)
    (shift_expr RIGHT_OP additive_expr)   : (list 'RSHIFT $1 $3)
    )

   (relational_expr
    (shift_expr)                       : $1
    (relational_expr < shift_expr)     : (list '< $1 $3)
    (relational_expr > shift_expr)     : (list '> $1 $3)
    (relational_expr LE_OP shift_expr) : (list 'LE $1 $3)
    (relational_expr GE_OP shift_expr) : (list 'GE $1 $3)
    )

   (equality_expr
    (relational_expr)                        : $1
    (equality_expr EQ_OP relational_expr)    : (list 'EQ $1 $3)
    (equality_expr NE_OP relational_expr)    : (lsit 'NEQ $1 $3)
    )

   (and_expr
    (equality_expr)              : $1
    (and_expr & equality_expr)   : (list 'LOGAND $1 $3)
    )


   (exclusive_or_expr
    (and_expr)                     : $1
    (exclusive_or_expr ^ and_expr) : (list 'LOGXOR $1 $3)
    )

   (inclusive_or_expr
    (exclusive_or_expr)            : $1
    (inclusive_or_expr OR exclusive_or_expr) : (list 'LOGIOR $1 $3)
    )


   (logical_and_expr
    (inclusive_or_expr)            : $1
    (logical_and_expr AND_OP inclusive_or_expr)
    : (list 'AND $1 $3)
    )

   (logical_or_expr
    (logical_and_expr) : $1
    (logical_or_expr OR_OP logical_and_expr)
    : (list 'OR $1 $3)
    )

   (conditional_expr
    (logical_or_expr) : $1
    (logical_or_expr ? logical_or_expr COLON conditional_expr)
    : (list 'COND $1 $3 $5)
    )

   (assignment_expr
    (conditional_expr) : $1
    (unary_expr assignment_operator assignment_expr)
    : (list 'ASSGIN $1 $2 $3)
    )


   (assignment_operator
    (=)             : 'NOP
    (MUL_ASSIGN)    : 'MUL
    (DIV_ASSIGN)    : 'DIV
    (MOD_ASSIGN)    : 'MOD
    (ADD_ASSIGN)    : 'AND
    (SUB_ASSIGN)    : 'SUB
    (LEFT_ASSIGN)   : 'LSHIFT
    (RIGHT_ASSIGN)  : 'RSHIFT
    (AND_ASSIGN)    : 'AND
    (XOR_ASSIGN)    : 'XOR
    (OR_ASSIGN)     : 'OR
    )

   (expr
    (assignment_expr) : $1
    (expr COMMA assignment_expr) : (append $1 $3)
    )    
   

   (constant_expr
    (conditional_expr) : $1
    )

   (struct_or_union_specifier
    (struct_or_union identifier LCBRA struct_declaration_list RCBRA)
    : (list $1 $2 $4)
    (struct_or_union LCBRA struct_declaration_list RCBRA)
    : (list $1 'NIL $3)
    (struct_or_union identifier)
    : (list $1 $2 'NIL)
    )

   (struct_or_union
    (STRUCT) : 'STRUCT
    (UNION)  : 'UNION
    )

   (struct_declaration_list
    (struct_declaration) 
    : (list $1)
    (struct_declaration_list struct_declaration)
    : (append $1 (list $2))
    )

   (struct_declaration
    (type_specifier_list struct_declarator_list SEMICOLON)
    : (list 'struct_declaration $1 $2)
    )

   (struct_declarator_list
    (struct_declarator) : (list $1)
    (struct_declarator_list COMMA struct_declarator) : (append $1 (list $3))
    )

   (struct_declarator
    (declarator) : $1
    (COLON constant_expr) 
    : (list 'BITFIELD 'NIL $2)
    (declarator COLON constant_expr)
    : (list 'BITFIELD $1 $3)
    )

   (enum_specifier
    (ENUM LCBRA enumerator_list RCBRA)
    : (list 'ENUM 'NIL $3)
    (ENUM identifier LCBRA enumerator_list RCBRA)
    : (list 'ENUM $2 $4)
    (ENUM identifier)
    : (list 'ENUM $2 'NIL)
    )

   (enumerator_list
    (enumerator)                       : (list $1)
    (enumerator_list COMMA enumerator) : (append $1 (list $3))
    )

   (enumerator
    (identifier)                 : (list 'enumerator $1 'NIL)
    (identifier = constant_expr) : (list 'enumerator $1 $3)
    )

   (declarator
    (declarator2)         : (cons 'declarator $1)
    (pointer declarator2) : (cons 'declarator (append $1 $2))
    )

   (declarator2
    (identifier)
    : (list 'identifier $1)
    (LPAREN declarator RPAREN)
    : $2
    (declarator2 LSBRA RSBRA) 
    : (list 'ARRAY $1 'NIL)
    (declarator2 LSBRA constant_expr RSBRA)
    : (list 'ARRAY $1 $3)
    (declarator2 LPAREN RPAREN)
    : (list 'FUNCTION $1 'NIL)
    (declarator2 LPAREN parameter_type_list RPAREN)
    : (list 'FUNCTION $1 $3)
    (declarator2 LPAREN parameter_identifier_list RPAREN)
    : (list 'FUNCTION $1 $3)
    )

   (pointer
    (*) : '(POINTER)
    (* type_specifier_list) : (cons 'POINTER $2)
    (* pointer)             : (cons 'POINTER $2)
    (* type_specifier_list pointer) : (cons 'POINTER (append $2 $3))
    )

   (type_specifier_list
    (type_specifier) : (list $1)
    (type_specifier_list type_specifier) : (append $1 (list $2))
    )

   (parameter_identifier_list
    (identifier_list)          : $1
    (identifier_list COMMA ELLIPSIS) : (append $1 '(OPT))
    )
   
   (identifier_list
    (identifier) : (list $1)
    (identifier_list COMMA identifier) : (append $1 (list $3))
    )

   (parameter_type_list
    (parameter_list) : $1
    (parameter_list COMMA ELLIPSIS) : (append $1 '(OPT))
    )

   (parameter_list
    (parameter_declaration) : (list $1)
    (parameter_list COMMA parameter_declaration)
    : (append $1 (list $3))
    )

   (parameter_declaration
    (type_specifier_list declarator) : (list 'parameter_declaration $1 $2)
    (type_name) : (list 'parameter_declaration $1 'NIL)
    )

   (type_name
    (type_specifier_list) : (list 'type_name $1 'NIL)
    (type_specifier_list abstract_declarator) : (list 'type_name $1 $2)
    )

   (abstract_declarator
    (pointer)
    : (list 'abstract_declarator $1 'NIL)
    (abstract_declarator2) 
    : (list 'abstract_declarator 'NIL $1)
    (pointer abstract_declarator2) 
    : (list 'abstract_declarator $1 $2)
    )


   (abstract_declarator2
    (LPAREN abstract_declarator RPAREN) : $2
    (LSBRA RSBRA) 
    : (list 'ARRAY 'NIL 'NIL)
    (LSBRA constant_expr RSBRA)
    : (list 'ARRAY 'NIL $2)
    (abstract_declarator2 LSBRA RSBRA)
    : (list 'ARRAY $1 'NIL)
    (abstract_declarator2 LSBRA constant_expr RSBRA)
    : (list 'ARRAY $1 $3)
    (LPAREN RPAREN)
    : (list 'FUNCTION 'NIL 'NIL)
    (LPAREN parameter_type_list RPAREN)
    : (list 'FUNCTION 'NIL $2)
    (abstract_declarator2 LPAREN RPAREN)
    : (list 'FUNCTION $1 'NIL)
    (abstract_declarator2 LPAREN parameter_type_list RPAREN)
    : (list 'FUNCTION $1 $3)
    )

   (initializer
    (assignment_expr) : (list 'initializer $1)
    (LCBRA initializer_list RCBRA) 
    : (list 'initializer $2)
    (LCBRA initializer_list COMMA RCBRA)
    : (list 'initializer $2)
    )

   (initializer_list
    (initializer) : (list $1)
    (initializer_list COMMA initializer) : (append $1 (list $3))
    )

   (statement
    (labeled_statement)    : $1
    (compound_statement)   : $1
    (expression_statement) : $1
    (selection_statement)  : $1
    (iteration_statement)  : $1
    (jump_statement)       : $1
    )

   (labeled_statement
    (identifier COLON statement) 
    : (list 'LABEL $1 $3)
    (CASE constant_expr COLON statement)
    : (list 'CASE $1 $4)
    (DEFAULT COLON statement)
    : (list 'DEFAULT $3)
    )

   (compound_statement
    (LCBRA RCBRA)
    : (list 'compound_statement 'NIL 'NIL)
    (LCBRA statement_list RCBRA) 
    : (list 'compound_statement 'NIL $2)
    (LCBRA declaration_list RCBRA) 
    : (list 'compound_statement $2 'NIL)
    (LCBRA declaration_list statement_list RCBRA)
    : (list 'compound_statement $2 $3)
    (error RCBRA)
    )

   (declaration_list
    (declaration) : (list $1)
    (declaration_list declaration) : (append $1 (list $2))
    )

   (statement_list
    (statement) : (list $1)
    (statement_list statement) : (append $1 (list $2))
    )

   (expression_statement
    (SEMICOLON) : (list 'expression_statement)
    (expr SEMICOLON) : (list 'expression_statement $1)
    (error SEMICOLON) 
    )

   (selection_statement
    (IF LPAREN expr RPAREN statement)
    : (list 'IF $3 $4 'NIL)
    (IF LPAREN expr RPAREN statement ELSE statement)
    : (list 'IF $3 $4 $6)
    (SWITCH LPAREN expr RPAREN statement)
    : (list 'SWITCH $3 $5)
    )

   (iteration_statement
    (WHILE LPAREN expr RPAREN statement)
    : (list 'WHILE $3 $5)
    (DO statement WHILE LPAREN expr RPAREN SEMICOLON)
    : (list 'DO $2 $5)
    (FOR LPAREN SEMICOLON SEMICOLON RPAREN statement)
    : (list 'FOR 'NIL 'NIL 'NIL $6)
    (FOR LPAREN SEMICOLON SEMICOLON expr RPAREN statement)
    : (list 'FOR 'NIL 'NIL $5 $7)
    (FOR LPAREN SEMICOLON expr SEMICOLON RPAREN statement)
    : (list 'FOR 'NIL $4 'NIL $7)
    (FOR LPAREN SEMICOLON expr SEMICOLON expr RPAREN statement)
    : (list 'FOR 'NIL $4 $6 $8)
    (FOR LPAREN expr SEMICOLON SEMICOLON RPAREN statement)
    : (list 'FOR $3 'NIL 'NIL  $7)
    (FOR LPAREN expr SEMICOLON SEMICOLON expr RPAREN statement)
    : (list 'FOR $3 'NIL $6 $8)
    (FOR LPAREN expr SEMICOLON expr SEMICOLON RPAREN statement)
    : (list 'FOR $3 $5 'NIL $8)
    (FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN statement)
    : (list 'FOR $3 $5 $7 $9)
    )

   (jump_statement
    (GOTO identifier SEMICOLON) : (list 'GOTO $2)
    (CONTINUE SEMICOLON)        : (list 'CONTINUE)
    (BREAK SEMICOLON)           : (list 'BREAK)
    (RETURN SEMICOLON)          : (list 'RETURN)
    (RETURN expr SEMICOLON)     : (list 'RETURN $2)
    )

   (identifier
    (IDENTIFIER)  : $1
 ;  (TYPENAME)
    )))

(define (pppp v)
  (define (ff v n)
    (let ((sp (make-string n #\space)))
      (define (wri x) (display sp) (write x) (newline))
      (define (dsp x) (display sp) (display x) (newline))
      (for-each (lambda (x)
                  (if (pair? x)
                      (begin 
                        (dsp "(")
                        (ff x (+ n 4))
                        (dsp ")"))
                      (wri x)))
                v)))
  (newline)
  (display "(")(newline)
  (ff v 1)
  (display ")")(newline)
  )

(define (do-decl v)
  (define (typ x l)
    (cond ((eq? (car x) 'IDENTIFIER)
           l)
          ((pair? (car x))
           (typ (car x) l))
          (else
           (typ (cdr x) (cons (car x) l)))))

  (if (not (eq? (car v) 'declaration)) (error "something is wrong"))
  (let ((x (cadr v))
        (y (caddr v)))
    (if (eq? (car x) 'TYPEDEF)
        (install-type x y))))

(add-load-path ".")
(load "c-scan.scm")

(define (main args)
  (with-input-from-port/mirroring-to-port
   (current-input-port)
   (current-output-port)
   (lambda () 
     (c-decl (lambda ()
               (let ((r (c-scan)))
                 ;;(display "LEX: ") (write r) (newline)
                 r))
             error)))
  0)

; EOF