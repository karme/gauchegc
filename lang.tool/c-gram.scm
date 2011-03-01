;(use lalr)

(define c-gram
  (lalr-parser
   (expect: 1)  ; IF-ELSE
   (output:    c-gram "c-gram.yy.scm")
   (out-table: "c-gram.out")
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
    (function_definition)     : (begin (newline) (ppvec $1) (newline))
    (declaration)             : (begin (newline) (ppvec $1) (newline))
    )

   (function_definition
    (declarator function_body)
    (declaration_specifiers declarator function_body)
    )

   (function_body
    (compound_statement)
    (declaration_list compound_statement)
    )

   (declaration
    (declaration_specifiers SEMICOLON)
    (declaration_specifiers init_declarator_list SEMICOLON) 
    )

   (declaration_specifiers
    (storage_class_specifier)
    (storage_class_specifier declaration_specifiers)
    (type_specifier)
    (type_specifier declaration_specifiers)
    )

   (init_declarator_list
    (init_declarator)
    (init_declarator_list COMMA init_declarator)
    )

   (init_declarator
    (declarator)               
    (declarator = initializer) 
    )

   (storage_class_specifier
    (TYPEDEF)  : 'TYPEDEF
    (EXTERN)
    (STATIC)
    (AUTO)
    (REGISTER)
    )

   (type_specifier
    (CHAR)
    (SHORT)
    (INT)
    (LONG)
    (SIGNED)
    (UNSIGNED)
    (FLOAT)
    (DOUBLE)
    (CONST)
    (VOLATILE)
    (VOID)
    (struct_or_union_specifier)
    (enum_specifier)
    (TYPENAME)
    )




   (primary_expr 
    (identifier)
    (CONSTANT)
    (STRING)
    (LPAREN expr RPAREN)
    )

   (postfix_expr 
    (primary_expr)
    (postfix_expr LSBRA expr RSBRA)
    (postfix_expr LPAREN RPAREN)
    (postfix_expr LPAREN argument_expr_list RPAREN)
    (postfix_expr DOT identifier)
    (postfix_expr PTR_OP identifier)
    (postfix_expr INC_OP)
    (postfix_expr DEC_OP)
    )

   (argument_expr_list
    (assignment_expr)
    (argument_expr_list COMMA assignment_expr)
    )

   (unary_expr 
    (postfix_expr)
    (INC_OP unary_expr)
    (DEC_OP unary_expr)
    (unary_operator cast_expr)
    (SIZEOF unary_expr)
    (SIZEOF LPAREN type_name RPAREN)
    )

   (unary_operator 
    (&)
    (*)
    (+)
    (-)
    (~)
    (!)
    )

   (cast_expr
    (unary_expr)
    (LPAREN type_name RPAREN cast_expr)
    )

   (multiplicative_expr
    (cast_expr)
    (multiplicative_expr * cast_expr)
    (multiplicative_expr / cast_expr)
    (multiplicative_expr % cast_expr)
    )

   (additive_expr
    (multiplicative_expr)
    (additive_expr + multiplicative_expr)
    (additive_expr - multiplicative_expr)
    )

   (shift_expr
    (additive_expr)
    (shift_expr LEFT_OP additive_expr)
    (shift_expr RIGHT_OP additive_expr)
    )

   (relational_expr
    (shift_expr)
    (relational_expr < shift_expr)
    (relational_expr > shift_expr)
    (relational_expr LE_OP shift_expr)
    (relational_expr GE_OP shift_expr)
    )

   (equality_expr
    (relational_expr)
    (equality_expr EQ_OP relational_expr)
    (equality_expr NE_OP relational_expr)
    )

   (and_expr
    (equality_expr)
    (and_expr & equality_expr)
    )


   (exclusive_or_expr
    (and_expr)
    (exclusive_or_expr ^ and_expr)
    )


   (inclusive_or_expr
    (exclusive_or_expr)
    (inclusive_or_expr OR exclusive_or_expr)
    )


   (logical_and_expr
    (inclusive_or_expr)
    (logical_and_expr AND_OP inclusive_or_expr)
    )

   (logical_or_expr
    (logical_and_expr)
    (logical_or_expr OR_OP logical_and_expr)
    )

   (conditional_expr
    (logical_or_expr)
    (logical_or_expr ? logical_or_expr COLON conditional_expr)
    )

   (assignment_expr
    (conditional_expr)
    (unary_expr assignment_operator assignment_expr)
    )


   (assignment_operator
    (=)
    (MUL_ASSIGN)
    (DIV_ASSIGN)
    (MOD_ASSIGN)
    (ADD_ASSIGN)
    (SUB_ASSIGN)
    (LEFT_ASSIGN)
    (RIGHT_ASSIGN)
    (AND_ASSIGN)
    (XOR_ASSIGN)
    (OR_ASSIGN)
    )

   (expr
    (assignment_expr)
    (expr COMMA assignment_expr)
    )    
   

   (constant_expr
    (conditional_expr)
    )

   (struct_or_union_specifier
    (struct_or_union identifier LCBRA struct_declaration_list RCBRA)
    (struct_or_union LCBRA struct_declaration_list RCBRA)
    (struct_or_union identifier)
    )

   (struct_or_union
    (STRUCT)
    (UNION)
    )

   (struct_declaration_list
    (struct_declaration)
    (struct_declaration_list struct_declaration)
    )

   (struct_declaration
    (type_specifier_list struct_declarator_list SEMICOLON)
    )

   (struct_declarator_list
    (struct_declarator)
    (struct_declarator_list COMMA struct_declarator)
    )

   (struct_declarator
    (declarator)
    (COLON constant_expr)
    (declarator COLON constant_expr)
    )

   (enum_specifier
    (ENUM LCBRA enumerator_list RCBRA)
    (ENUM identifier LCBRA enumerator_list RCBRA)
    (ENUM identifier)
    )

   (enumerator_list
    (enumerator)
    (enumerator_list COMMA enumerator)
    )

   (enumerator
    (identifier)
    (identifier = constant_expr)
    )

   (declarator
    (declarator2)        
    (pointer declarator2)
    )

   (declarator2
    (identifier)         
    (LPAREN declarator RPAREN)
    (declarator2 LSBRA RSBRA) 
    (declarator2 LSBRA constant_expr RSBRA)
    (declarator2 LPAREN RPAREN)            
    (declarator2 LPAREN parameter_type_list RPAREN) 
    (declarator2 LPAREN parameter_identifier_list RPAREN)
    )

   (pointer
    (*)
    (* type_specifier_list)
    (* pointer)
    (* type_specifier_list pointer)
    )

   (type_specifier_list
    (type_specifier)
    (type_specifier_list type_specifier)
    )

   (parameter_identifier_list
    (identifier_list)
    (identifier_list COMMA ELLIPSIS)
    )
   
   (identifier_list
    (identifier)
    (identifier_list COMMA identifier)
    )

   (parameter_type_list
    (parameter_list)
    (parameter_list COMMA ELLIPSIS)
    )

   (parameter_list
    (parameter_declaration)
    (parameter_list COMMA parameter_declaration)
    )

   (parameter_declaration
    (type_specifier_list declarator)
    (type_name)
    )

   (type_name
    (type_specifier_list)
    (type_specifier_list abstract_declarator)
    )

   (abstract_declarator
    (pointer)
    (abstract_declarator2)
    (pointer abstract_declarator2)
    )


   (abstract_declarator2
    (LPAREN abstract_declarator RPAREN)
    (LSBRA RSBRA)
    (LSBRA constant_expr RSBRA)
    (abstract_declarator2 LSBRA RSBRA)
    (abstract_declarator2 LSBRA constant_expr RSBRA)
    (LPAREN RPAREN)
    (LPAREN parameter_type_list RPAREN)
    (abstract_declarator2 LPAREN RPAREN)
    (abstract_declarator2 LPAREN parameter_type_list RPAREN)
    )

   (initializer
    (assignment_expr)
    (LCBRA initializer_list RCBRA)
    (LCBRA initializer_list COMMA RCBRA)
    )

   (initializer_list
    (initializer)
    (initializer_list COMMA initializer)
    )

   (statement
    (labeled_statement)
    (compound_statement)
    (expression_statement)
    (selection_statement)
    (iteration_statement)
    (jump_statement)
    )

   (labeled_statement
    (identifier COLON statement)
    (CASE constant_expr COLON statement)
    (DEFAULT COLON statement)
    )

   (compound_statement
    (LCBRA RCBRA)
    (LCBRA statement_list RCBRA)
    (LCBRA declaration_list RCBRA)
    (LCBRA declaration_list statement_list RCBRA)
    (error RCBRA)
    )

   (declaration_list
    (declaration)
    (declaration_list declaration)
    )

   (statement_list
    (statement)
    (statement_list statement)
    )

   (expression_statement
    (SEMICOLON)
    (expr SEMICOLON)
    (error SEMICOLON)
    )

   (selection_statement
    (IF LPAREN expr RPAREN statement)
    (IF LPAREN expr RPAREN statement ELSE statement)
    (SWITCH LPAREN expr RPAREN statement)
    )

   (iteration_statement
    (WHILE LPAREN expr RPAREN statement)
    (DO statement WHILE LPAREN expr RPAREN SEMICOLON)
    (FOR LPAREN SEMICOLON SEMICOLON RPAREN statement)
    (FOR LPAREN SEMICOLON SEMICOLON expr RPAREN statement)
    (FOR LPAREN SEMICOLON expr SEMICOLON RPAREN statement)
    (FOR LPAREN SEMICOLON expr SEMICOLON expr RPAREN statement)
    (FOR LPAREN expr SEMICOLON SEMICOLON RPAREN statement)
    (FOR LPAREN expr SEMICOLON SEMICOLON expr RPAREN statement)
    (FOR LPAREN expr SEMICOLON expr SEMICOLON RPAREN statement)
    (FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN statement)
    )

   (jump_statement
    (GOTO identifier SEMICOLON)
    (CONTINUE SEMICOLON)
    (BREAK SEMICOLON)
    (RETURN SEMICOLON)
    (RETURN expr SEMICOLON)
    )

   (identifier
    (IDENTIFIER)
;   (TYPENAME)
    )))
