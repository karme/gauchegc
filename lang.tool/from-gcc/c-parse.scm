(define c-parse
  (lalr-parser 
   (expect:  13 )
   ;;(start:  program)
   (ID  SEMICOLON                       ;
        ;; For Scheme,  LPAREN LSBRA  are defined blow
        ;; COMMA=,  LCBRA={  RCBRA=} LSBRA=[  RSBRA=]
        ;; LPAREN=( RPAREN=) PIPE=| DOT=.
        COMMA RSBRA LCBRA RCBRA RPAREN ; DOT 
        
        ~ !

    IDENTIFIER
    TYPENAME
    SCSPEC			
    STATIC			
    TYPESPEC
    TYPE_QUAL
    CONSTANT
    STRING
    ELLIPSIS
    SIZEOF ENUM STRUCT UNION WHILE DO FOR SWITCH CASE DEFAULT ;  IF ELSE
    BREAK CONTINUE RETURN GOTO ASM_KEYWORD TYPEOF ALIGNOF
    ATTRIBUTE EXTENSION LABEL
    REALPART IMAGPART VA_ARG CHOOSE_EXPR TYPES_COMPATIBLE_P
    PTR_VALUE PTR_BASE PTR_EXTENT
    FUNC_NAME OFFSETOF
    (nonassoc:  IF)
    (nonassoc:  ELSE)
    (right:   ASSIGN  = )
    (right:    ?  COLON)
    (left:   OROR)
    (left:   ANDAND)
    (left:   OR)
    (left:    ^ )
    (left:    & )
    (left:   EQCOMPARE)
    (left:   ARITHCOMPARE)
    (left:   LSHIFT RSHIFT)
    (left:    +   - )
    (left:    *   /   % )
    (right:   UNARY PLUSPLUS MINUSMINUS)
    (left:  HYPERUNARY)
    (left:   POINTSAT  DOT  LPAREN LSBRA)
    AT_INTERFACE AT_IMPLEMENTATION AT_END AT_SELECTOR AT_DEFS AT_ENCODE
    CLASSNAME AT_PUBLIC AT_PRIVATE AT_PROTECTED AT_PROTOCOL 
    OBJECTNAME AT_CLASS AT_ALIAS
    AT_THROW AT_TRY AT_CATCH AT_FINALLY AT_SYNCHRONIZED
    OBJC_STRING
    )
   (program
    ()
    (extdefs)
    )
   (extdefs
    (extdef)
    (extdefs extdef)
    )
   (extdef
    (fndef)
    (datadef)
    (asmdef)
    (extension extdef)
    )
   (datadef
    (setspecs notype_initdecls SEMICOLON)
    (declspecs_nots setspecs notype_initdecls SEMICOLON)
    (declspecs_ts setspecs initdecls SEMICOLON)
    (declspecs SEMICOLON)
    (error SEMICOLON)
    (error RCBRA)
    (SEMICOLON)
    )
   (fndef
    (declspecs_ts setspecs declarator old_style_parm_decls save_location compstmt_or_error)
    ;;(declspecs_ts setspecs declarator error)
    (declspecs_nots setspecs notype_declarator old_style_parm_decls save_location compstmt_or_error)
    ;;(declspecs_nots setspecs notype_declarator error)
    (setspecs notype_declarator old_style_parm_decls save_location compstmt_or_error)
    ;;(setspecs notype_declarator error)
    )
   (identifier
    (IDENTIFIER)
    (TYPENAME)
    )
   (unop
    (&)
    (-)
    (+)
    (PLUSPLUS)
    (MINUSMINUS)
    (~)
    (!)
    )
   (expr
    (nonnull_exprlist)
    )
   (exprlist
    ()
    (nonnull_exprlist)
    )
   (nonnull_exprlist
    (expr_no_commas)
    (nonnull_exprlist COMMA expr_no_commas)
    )
   (unary_expr
    (primary)
    (* cast_expr (prec: UNARY))
    (extension cast_expr (prec: UNARY))
    (unop cast_expr (prec: UNARY))
    (ANDAND identifier)
    (sizeof unary_expr (prec: UNARY))
    (sizeof LPAREN typename RPAREN (prec: HYPERUNARY))
    (alignof unary_expr (prec: UNARY))
    (alignof LPAREN typename RPAREN (prec: HYPERUNARY))
    (REALPART cast_expr (prec: UNARY))
    (IMAGPART cast_expr (prec: UNARY))
    )
   (sizeof
    (SIZEOF)
    )
   (alignof
    (ALIGNOF)
    )
   (typeof
    (TYPEOF)
    )
   (cast_expr
    (unary_expr)
    (LPAREN typename RPAREN cast_expr (prec: UNARY))
    )
   (expr_no_commas
    (cast_expr)
    (expr_no_commas + expr_no_commas)
    (expr_no_commas - expr_no_commas)
    (expr_no_commas * expr_no_commas)
    (expr_no_commas / expr_no_commas)
    (expr_no_commas % expr_no_commas)
    (expr_no_commas LSHIFT expr_no_commas)
    (expr_no_commas RSHIFT expr_no_commas)
    (expr_no_commas ARITHCOMPARE expr_no_commas)
    (expr_no_commas EQCOMPARE expr_no_commas)
    (expr_no_commas & expr_no_commas)
    (expr_no_commas OR expr_no_commas)
    (expr_no_commas ^ expr_no_commas)
    (expr_no_commas ANDAND expr_no_commas)
    (expr_no_commas OROR expr_no_commas)
    (expr_no_commas ? expr COLON expr_no_commas)
    (expr_no_commas ? COLON expr_no_commas)
    (expr_no_commas = expr_no_commas)
    (expr_no_commas ASSIGN expr_no_commas)
    )
   (primary
    (IDENTIFIER)
    (CONSTANT)
    (STRING)
    (FUNC_NAME)
    (LPAREN typename RPAREN LCBRA initlist_maybe_comma RCBRA (prec: UNARY))
    (LPAREN expr RPAREN)
    ;;(LPAREN error RPAREN)
    (compstmt_primary_start compstmt_nostart RPAREN)
    ;;(compstmt_primary_start error RPAREN)
    (primary LPAREN exprlist RPAREN (prec: DOT))
    (VA_ARG LPAREN expr_no_commas COMMA typename RPAREN)
    (OFFSETOF LPAREN typename COMMA offsetof_member_designator RPAREN)
    ;;(OFFSETOF LPAREN error RPAREN)
    (CHOOSE_EXPR LPAREN expr_no_commas COMMA expr_no_commas COMMA expr_no_commas RPAREN)
    ;;(CHOOSE_EXPR LPAREN error RPAREN)
    (TYPES_COMPATIBLE_P LPAREN typename COMMA typename RPAREN)
    ;;(TYPES_COMPATIBLE_P LPAREN error RPAREN)
    (primary LSBRA expr RSBRA (prec: DOT))
    (primary DOT identifier)
    (primary POINTSAT identifier)
    (primary PLUSPLUS)
    (primary MINUSMINUS)
    )
   (offsetof_member_designator
    (identifier)
    (offsetof_member_designator DOT identifier)
    (offsetof_member_designator LSBRA expr RSBRA)
    )
   (old_style_parm_decls
    ()
    (datadecls)
    )
   (lineno_datadecl
    (save_location datadecl)
    )
   (datadecls
    (lineno_datadecl)
    (errstmt)
    (datadecls lineno_datadecl)
    (lineno_datadecl errstmt)
    )
   (datadecl
    (declspecs_ts_nosa setspecs initdecls SEMICOLON)
    (declspecs_nots_nosa setspecs notype_initdecls SEMICOLON)
    (declspecs_ts_nosa SEMICOLON)
    (declspecs_nots_nosa SEMICOLON)
    )
   (lineno_decl
    (save_location decl)
    )
   (setspecs
    ()
    )
   (maybe_resetattrs
    (maybe_attribute)
    )
   (decl
    (declspecs_ts setspecs initdecls SEMICOLON)
    (declspecs_nots setspecs notype_initdecls SEMICOLON)
    (declspecs_ts setspecs nested_function)
    (declspecs_nots setspecs notype_nested_function)
    (declspecs SEMICOLON)
    (extension decl)
    )
   (declspecs_nosc_nots_nosa_noea
    (TYPE_QUAL)
    (declspecs_nosc_nots_nosa_noea TYPE_QUAL)
    (declspecs_nosc_nots_nosa_ea TYPE_QUAL)
    )
   (declspecs_nosc_nots_nosa_ea
    (declspecs_nosc_nots_nosa_noea attributes)
    )
   (declspecs_nosc_nots_sa_noea
    (declspecs_nosc_nots_sa_noea TYPE_QUAL)
    (declspecs_nosc_nots_sa_ea TYPE_QUAL)
    )
   (declspecs_nosc_nots_sa_ea
    (attributes)
    (declspecs_nosc_nots_sa_noea attributes)
    )
   (declspecs_nosc_ts_nosa_noea
    (typespec_nonattr)
    (declspecs_nosc_ts_nosa_noea TYPE_QUAL)
    (declspecs_nosc_ts_nosa_ea TYPE_QUAL)
    (declspecs_nosc_ts_nosa_noea typespec_reserved_nonattr)
    (declspecs_nosc_ts_nosa_ea typespec_reserved_nonattr)
    (declspecs_nosc_nots_nosa_noea typespec_nonattr)
    (declspecs_nosc_nots_nosa_ea typespec_nonattr)
    )
   (declspecs_nosc_ts_nosa_ea
    (typespec_attr)
    (declspecs_nosc_ts_nosa_noea attributes)
    (declspecs_nosc_ts_nosa_noea typespec_reserved_attr)
    (declspecs_nosc_ts_nosa_ea typespec_reserved_attr)
    (declspecs_nosc_nots_nosa_noea typespec_attr)
    (declspecs_nosc_nots_nosa_ea typespec_attr)
    )
   (declspecs_nosc_ts_sa_noea
    (declspecs_nosc_ts_sa_noea TYPE_QUAL)
    (declspecs_nosc_ts_sa_ea TYPE_QUAL)
    (declspecs_nosc_ts_sa_noea typespec_reserved_nonattr)
    (declspecs_nosc_ts_sa_ea typespec_reserved_nonattr)
    (declspecs_nosc_nots_sa_noea typespec_nonattr)
    (declspecs_nosc_nots_sa_ea typespec_nonattr)
    )
   (declspecs_nosc_ts_sa_ea
    (declspecs_nosc_ts_sa_noea attributes)
    (declspecs_nosc_ts_sa_noea typespec_reserved_attr)
    (declspecs_nosc_ts_sa_ea typespec_reserved_attr)
    (declspecs_nosc_nots_sa_noea typespec_attr)
    (declspecs_nosc_nots_sa_ea typespec_attr)
    )
   (declspecs_sc_nots_nosa_noea
    (scspec)
    (declspecs_sc_nots_nosa_noea TYPE_QUAL)
    (declspecs_sc_nots_nosa_ea TYPE_QUAL)
    (declspecs_nosc_nots_nosa_noea scspec)
    (declspecs_nosc_nots_nosa_ea scspec)
    (declspecs_sc_nots_nosa_noea scspec)
    (declspecs_sc_nots_nosa_ea scspec)
    )
   (declspecs_sc_nots_nosa_ea
    (declspecs_sc_nots_nosa_noea attributes)
    )
   (declspecs_sc_nots_sa_noea
    (declspecs_sc_nots_sa_noea TYPE_QUAL)
    (declspecs_sc_nots_sa_ea TYPE_QUAL)
    (declspecs_nosc_nots_sa_noea scspec)
    (declspecs_nosc_nots_sa_ea scspec)
    (declspecs_sc_nots_sa_noea scspec)
    (declspecs_sc_nots_sa_ea scspec)
    )
   (declspecs_sc_nots_sa_ea
    (declspecs_sc_nots_sa_noea attributes)
    )
   (declspecs_sc_ts_nosa_noea
    (declspecs_sc_ts_nosa_noea TYPE_QUAL)
    (declspecs_sc_ts_nosa_ea TYPE_QUAL)
    (declspecs_sc_ts_nosa_noea typespec_reserved_nonattr)
    (declspecs_sc_ts_nosa_ea typespec_reserved_nonattr)
    (declspecs_sc_nots_nosa_noea typespec_nonattr)
    (declspecs_sc_nots_nosa_ea typespec_nonattr)
    (declspecs_nosc_ts_nosa_noea scspec)
    (declspecs_nosc_ts_nosa_ea scspec)
    (declspecs_sc_ts_nosa_noea scspec)
    (declspecs_sc_ts_nosa_ea scspec)
    )
   (declspecs_sc_ts_nosa_ea
    (declspecs_sc_ts_nosa_noea attributes)
    (declspecs_sc_ts_nosa_noea typespec_reserved_attr)
    (declspecs_sc_ts_nosa_ea typespec_reserved_attr)
    (declspecs_sc_nots_nosa_noea typespec_attr)
    (declspecs_sc_nots_nosa_ea typespec_attr)
    )
   (declspecs_sc_ts_sa_noea
    (declspecs_sc_ts_sa_noea TYPE_QUAL)
    (declspecs_sc_ts_sa_ea TYPE_QUAL)
    (declspecs_sc_ts_sa_noea typespec_reserved_nonattr)
    (declspecs_sc_ts_sa_ea typespec_reserved_nonattr)
    (declspecs_sc_nots_sa_noea typespec_nonattr)
    (declspecs_sc_nots_sa_ea typespec_nonattr)
    (declspecs_nosc_ts_sa_noea scspec)
    (declspecs_nosc_ts_sa_ea scspec)
    (declspecs_sc_ts_sa_noea scspec)
    (declspecs_sc_ts_sa_ea scspec)
    )
   (declspecs_sc_ts_sa_ea
    (declspecs_sc_ts_sa_noea attributes)
    (declspecs_sc_ts_sa_noea typespec_reserved_attr)
    (declspecs_sc_ts_sa_ea typespec_reserved_attr)
    (declspecs_sc_nots_sa_noea typespec_attr)
    (declspecs_sc_nots_sa_ea typespec_attr)
    )
   (declspecs_ts
    (declspecs_nosc_ts_nosa_noea)
    (declspecs_nosc_ts_nosa_ea)
    (declspecs_nosc_ts_sa_noea)
    (declspecs_nosc_ts_sa_ea)
    (declspecs_sc_ts_nosa_noea)
    (declspecs_sc_ts_nosa_ea)
    (declspecs_sc_ts_sa_noea)
    (declspecs_sc_ts_sa_ea)
    )
   (declspecs_nots
    (declspecs_nosc_nots_nosa_noea)
    (declspecs_nosc_nots_nosa_ea)
    (declspecs_nosc_nots_sa_noea)
    (declspecs_nosc_nots_sa_ea)
    (declspecs_sc_nots_nosa_noea)
    (declspecs_sc_nots_nosa_ea)
    (declspecs_sc_nots_sa_noea)
    (declspecs_sc_nots_sa_ea)
    )
   (declspecs_ts_nosa
    (declspecs_nosc_ts_nosa_noea)
    (declspecs_nosc_ts_nosa_ea)
    (declspecs_sc_ts_nosa_noea)
    (declspecs_sc_ts_nosa_ea)
    )
   (declspecs_nots_nosa
    (declspecs_nosc_nots_nosa_noea)
    (declspecs_nosc_nots_nosa_ea)
    (declspecs_sc_nots_nosa_noea)
    (declspecs_sc_nots_nosa_ea)
    )
   (declspecs_nosc_ts
    (declspecs_nosc_ts_nosa_noea)
    (declspecs_nosc_ts_nosa_ea)
    (declspecs_nosc_ts_sa_noea)
    (declspecs_nosc_ts_sa_ea)
    )
   (declspecs_nosc_nots
    (declspecs_nosc_nots_nosa_noea)
    (declspecs_nosc_nots_nosa_ea)
    (declspecs_nosc_nots_sa_noea)
    (declspecs_nosc_nots_sa_ea)
    )
   (declspecs_nosc
    (declspecs_nosc_ts_nosa_noea)
    (declspecs_nosc_ts_nosa_ea)
    (declspecs_nosc_ts_sa_noea)
    (declspecs_nosc_ts_sa_ea)
    (declspecs_nosc_nots_nosa_noea)
    (declspecs_nosc_nots_nosa_ea)
    (declspecs_nosc_nots_sa_noea)
    (declspecs_nosc_nots_sa_ea)
    )
   (declspecs
    (declspecs_nosc_nots_nosa_noea)
    (declspecs_nosc_nots_nosa_ea)
    (declspecs_nosc_nots_sa_noea)
    (declspecs_nosc_nots_sa_ea)
    (declspecs_nosc_ts_nosa_noea)
    (declspecs_nosc_ts_nosa_ea)
    (declspecs_nosc_ts_sa_noea)
    (declspecs_nosc_ts_sa_ea)
    (declspecs_sc_nots_nosa_noea)
    (declspecs_sc_nots_nosa_ea)
    (declspecs_sc_nots_sa_noea)
    (declspecs_sc_nots_sa_ea)
    (declspecs_sc_ts_nosa_noea)
    (declspecs_sc_ts_nosa_ea)
    (declspecs_sc_ts_sa_noea)
    (declspecs_sc_ts_sa_ea)
    )
   (maybe_type_quals_attrs
    ()
    (declspecs_nosc_nots)
    )
   (typespec_nonattr
    (typespec_reserved_nonattr)
    (typespec_nonreserved_nonattr)
    )
   (typespec_attr
    (typespec_reserved_attr)
    )
   (typespec_reserved_nonattr
    (TYPESPEC)
    (structsp_nonattr)
    )
   (typespec_reserved_attr
    (structsp_attr)
    )
   (typespec_nonreserved_nonattr
    (TYPENAME)
    (typeof LPAREN expr RPAREN)
    (typeof LPAREN typename RPAREN)
    )
   (initdecls
    (initdcl)
    (initdecls COMMA maybe_resetattrs initdcl)
    )
   (notype_initdecls
    (notype_initdcl)
    (notype_initdecls COMMA maybe_resetattrs notype_initdcl)
    )
   (initdcl
    (declarator maybeasm maybe_attribute = init)
    (declarator maybeasm maybe_attribute)
    )
   (notype_initdcl
    (notype_declarator maybeasm maybe_attribute = init)
    (notype_declarator maybeasm maybe_attribute)
    )
   (maybe_attribute
    ()
    (attributes)
    )
   (attributes
    (attribute)
    (attributes attribute)
    )
   (attribute
    (ATTRIBUTE stop_string_translation LPAREN LPAREN attribute_list RPAREN RPAREN start_string_translation)
    ;;(ATTRIBUTE error start_string_translation)
    )
   (attribute_list
    (attrib)
    (attribute_list COMMA attrib)
    )
   (attrib
    ()
    (any_word)
    (any_word LPAREN IDENTIFIER RPAREN)
    (any_word LPAREN IDENTIFIER COMMA nonnull_exprlist RPAREN)
    (any_word LPAREN exprlist RPAREN)
    )
   (any_word
    (identifier)
    (scspec)
    (TYPESPEC)
    (TYPE_QUAL)
    )
   (scspec
    (STATIC)
    (SCSPEC)
    )
   (init
    (expr_no_commas)
    (LCBRA initlist_maybe_comma RCBRA)
    ;;(error)
    )
   (initlist_maybe_comma
    ()
    (initlist1 maybecomma)
    )
   (initlist1
    (initelt)
    (initlist1 COMMA initelt)
    )
   (initelt
    (designator_list = initval)
    (designator initval)
    (identifier COLON initval)
    (initval)
    )
   (initval
    (LCBRA initlist_maybe_comma RCBRA)
    (expr_no_commas)
    ;;(error)
    )
   (designator_list
    (designator)
    (designator_list designator)
    )
   (designator
    (DOT identifier)
    (LSBRA expr_no_commas ELLIPSIS expr_no_commas RSBRA)
    (LSBRA expr_no_commas RSBRA)
    )
   (nested_function
    (declarator old_style_parm_decls save_location compstmt)
    )
   (notype_nested_function
    (notype_declarator old_style_parm_decls save_location compstmt)
    )
   (declarator
    (after_type_declarator)
    (notype_declarator)
    )
   (after_type_declarator
    (LPAREN maybe_attribute after_type_declarator RPAREN)
    (after_type_declarator LPAREN parmlist_or_identifiers (prec: DOT))
    (after_type_declarator array_declarator (prec: DOT))
    (* maybe_type_quals_attrs after_type_declarator (prec: UNARY))
    (TYPENAME)
    )
   (parm_declarator
    (parm_declarator_starttypename)
    (parm_declarator_nostarttypename)
    )
   (parm_declarator_starttypename
    (parm_declarator_starttypename LPAREN parmlist_or_identifiers (prec: DOT))
    (parm_declarator_starttypename array_declarator (prec: DOT))
    (TYPENAME)
    )
   (parm_declarator_nostarttypename
    (parm_declarator_nostarttypename LPAREN parmlist_or_identifiers (prec: DOT))
    (parm_declarator_nostarttypename array_declarator (prec: DOT))
    (* maybe_type_quals_attrs parm_declarator_starttypename (prec: UNARY))
    (* maybe_type_quals_attrs parm_declarator_nostarttypename (prec: UNARY))
    (LPAREN maybe_attribute parm_declarator_nostarttypename RPAREN)
    )
   (notype_declarator
    (notype_declarator LPAREN parmlist_or_identifiers (prec: DOT))
    (LPAREN maybe_attribute notype_declarator RPAREN)
    (* maybe_type_quals_attrs notype_declarator (prec: UNARY))
    (notype_declarator array_declarator (prec: DOT))
    (IDENTIFIER)
    )
   (struct_head
    (STRUCT)
    (STRUCT attributes)
    )
   (union_head
    (UNION)
    (UNION attributes)
    )
   (enum_head
    (ENUM)
    (ENUM attributes)
    )
   (structsp_attr
    (struct_head identifier LCBRA component_decl_list RCBRA maybe_attribute)
    (struct_head LCBRA component_decl_list RCBRA maybe_attribute)
    (union_head identifier LCBRA component_decl_list RCBRA maybe_attribute)
    (union_head LCBRA component_decl_list RCBRA maybe_attribute)
    (enum_head identifier LCBRA enumlist maybecomma_warn RCBRA maybe_attribute)
    (enum_head LCBRA enumlist maybecomma_warn RCBRA maybe_attribute)
    )
   (structsp_nonattr
    (struct_head identifier)
    (union_head identifier)
    (enum_head identifier)
    )
   (maybecomma
    ()
    (COMMA)
    )
   (maybecomma_warn
    ()
    (COMMA)
    )
   (component_decl_list
    (component_decl_list2)
    (component_decl_list2 component_decl)
    )
   (component_decl_list2
    ()
    (component_decl_list2 component_decl SEMICOLON)
    (component_decl_list2 SEMICOLON)
    )
   (component_decl
    (declspecs_nosc_ts setspecs components)
    (declspecs_nosc_ts setspecs)
    (declspecs_nosc_nots setspecs components_notype)
    (declspecs_nosc_nots)
    ;;(error)
    (extension component_decl)
    )
   (components
    (component_declarator)
    (components COMMA maybe_resetattrs component_declarator)
    )
   (components_notype
    (component_notype_declarator)
    (components_notype COMMA maybe_resetattrs component_notype_declarator)
    )
   (component_declarator
    (declarator maybe_attribute)
    (declarator COLON expr_no_commas maybe_attribute)
    (COLON expr_no_commas maybe_attribute)
    )
   (component_notype_declarator
    (notype_declarator maybe_attribute)
    (notype_declarator COLON expr_no_commas maybe_attribute)
    (COLON expr_no_commas maybe_attribute)
    )
   (enumlist
    (enumerator)
    (enumlist COMMA enumerator)
    ;;(error)
    )
   (enumerator
    (identifier)
    (identifier = expr_no_commas)
    )
   (typename
    (declspecs_nosc absdcl)
    )
   (absdcl
    ()
    (absdcl1)
    )
   (absdcl_maybe_attribute
    ()
    (absdcl1)
    (absdcl1_noea attributes)
    )
   (absdcl1
    (absdcl1_ea)
    (absdcl1_noea)
    )
   (absdcl1_noea
    (direct_absdcl1)
    (* maybe_type_quals_attrs absdcl1_noea)
    )
   (absdcl1_ea
    (* maybe_type_quals_attrs)
    (* maybe_type_quals_attrs absdcl1_ea)
    )
   (direct_absdcl1
    (LPAREN maybe_attribute absdcl1 RPAREN)
    (direct_absdcl1 LPAREN parmlist)
    (direct_absdcl1 array_declarator)
    (LPAREN parmlist)
    (array_declarator)
    )
   (array_declarator
    (LSBRA maybe_type_quals_attrs expr_no_commas RSBRA)
    (LSBRA maybe_type_quals_attrs RSBRA)
    (LSBRA maybe_type_quals_attrs * RSBRA)
    (LSBRA STATIC maybe_type_quals_attrs expr_no_commas RSBRA)
    (LSBRA declspecs_nosc_nots STATIC expr_no_commas RSBRA)
    )
   (stmts_and_decls
    (lineno_stmt_decl_or_labels_ending_stmt)
    (lineno_stmt_decl_or_labels_ending_decl)
    (lineno_stmt_decl_or_labels_ending_label)
    (lineno_stmt_decl_or_labels_ending_error)
    )
   (lineno_stmt_decl_or_labels_ending_stmt
    (lineno_stmt)
    (lineno_stmt_decl_or_labels_ending_stmt lineno_stmt)
    (lineno_stmt_decl_or_labels_ending_decl lineno_stmt)
    (lineno_stmt_decl_or_labels_ending_label lineno_stmt)
    (lineno_stmt_decl_or_labels_ending_error lineno_stmt)
    )
   (lineno_stmt_decl_or_labels_ending_decl
    (lineno_decl)
    (lineno_stmt_decl_or_labels_ending_stmt lineno_decl)
    (lineno_stmt_decl_or_labels_ending_decl lineno_decl)
    (lineno_stmt_decl_or_labels_ending_error lineno_decl)
    )
   (lineno_stmt_decl_or_labels_ending_label
    (lineno_label)
    (lineno_stmt_decl_or_labels_ending_stmt lineno_label)
    (lineno_stmt_decl_or_labels_ending_decl lineno_label)
    (lineno_stmt_decl_or_labels_ending_label lineno_label)
    (lineno_stmt_decl_or_labels_ending_error lineno_label)
    )
   (lineno_stmt_decl_or_labels_ending_error
    (errstmt)
    (lineno_stmt_decl_or_labels errstmt)
    )
   (lineno_stmt_decl_or_labels
    (lineno_stmt_decl_or_labels_ending_stmt)
    (lineno_stmt_decl_or_labels_ending_decl)
    (lineno_stmt_decl_or_labels_ending_label)
    (lineno_stmt_decl_or_labels_ending_error)
    )
   (errstmt
    (error SEMICOLON)
    )
   (c99_block_start
    ()
    )
   (maybe_label_decls
    ()
    (label_decls)
    )
   (label_decls
    (label_decl)
    (label_decls label_decl)
    )
   (label_decl
    (LABEL identifiers_or_typenames SEMICOLON)
    )
   (compstmt_or_error
    (compstmt)
    ;;(error compstmt)
    )
   (compstmt_start
    (LCBRA)
    )
   (compstmt_nostart
    (RCBRA)
    (maybe_label_decls compstmt_contents_nonempty RCBRA)
    )
   (compstmt_contents_nonempty
    (stmts_and_decls)
    ;;(error)
    )
   (compstmt_primary_start
    (LPAREN LCBRA)
    )
   (compstmt
    (compstmt_start compstmt_nostart)
    )
   (save_location
    ()
    )
   (lineno_labels
    ()
    (lineno_labels lineno_label)
    )
   (c99_block_lineno_labeled_stmt
    (c99_block_start lineno_labels lineno_stmt)
    )
   (lineno_stmt
    (save_location stmt)
    )
   (lineno_label
    (save_location label)
    )
   (condition
    (save_location expr)
    )
   (if_statement_1
    (c99_block_start lineno_labels if_statement)
    )
   (if_statement_2
    (c99_block_start lineno_labels SEMICOLON)
    (c99_block_lineno_labeled_stmt)
    )
   (if_statement
    (IF c99_block_start save_location LPAREN condition RPAREN if_statement_1 ELSE if_statement_2)
    (IF c99_block_start save_location LPAREN condition RPAREN if_statement_2 ELSE if_statement_2)
    (IF c99_block_start save_location LPAREN condition RPAREN if_statement_1 (prec: IF))
    (IF c99_block_start save_location LPAREN condition RPAREN if_statement_2 (prec: IF))
    )
   (start_break
    ()
    )
   (start_continue
    ()
    )
   (while_statement
    (WHILE c99_block_start save_location LPAREN condition RPAREN start_break start_continue c99_block_lineno_labeled_stmt)
    )
   (do_statement
    (DO c99_block_start save_location start_break start_continue c99_block_lineno_labeled_stmt WHILE LPAREN condition RPAREN SEMICOLON)
    )
   (xexpr
    ()
    (expr)
    )
   (for_init_stmt
    (xexpr SEMICOLON)
    (decl)
    )
   (for_cond_expr
    (save_location xexpr)
    )
   (for_incr_expr
    (xexpr)
    )
   (for_statement
    (FOR c99_block_start LPAREN for_init_stmt save_location for_cond_expr SEMICOLON for_incr_expr RPAREN start_break start_continue c99_block_lineno_labeled_stmt)
    )
   (switch_statement
    (SWITCH c99_block_start LPAREN expr RPAREN start_break c99_block_lineno_labeled_stmt)
    )
   (stmt_nocomp
    (expr SEMICOLON)
    (if_statement)
    (while_statement)
    (do_statement)
    (for_statement)
    (switch_statement)
    (BREAK SEMICOLON)
    (CONTINUE SEMICOLON)
    (RETURN SEMICOLON)
    (RETURN expr SEMICOLON)
    (asm_stmt)
    (GOTO identifier SEMICOLON)
    (GOTO * expr SEMICOLON)
    (SEMICOLON)
    )
   (stmt
    (compstmt)
    (stmt_nocomp)
    )
   (label
    (CASE expr_no_commas COLON)
    (CASE expr_no_commas ELLIPSIS expr_no_commas COLON)
    (DEFAULT COLON)
    (identifier save_location COLON maybe_attribute)
    )
   (simple_asm_expr
    (ASM_KEYWORD stop_string_translation LPAREN STRING RPAREN start_string_translation)
    )
   (maybeasm
    ()
    (simple_asm_expr)
    )
   (asmdef
    (simple_asm_expr SEMICOLON)
    ;;(ASM_KEYWORD error start_string_translation SEMICOLON)
    )
   (asm_stmt
    (ASM_KEYWORD maybe_volatile stop_string_translation LPAREN asm_argument RPAREN start_string_translation SEMICOLON)
    )
   (asm_argument
    (STRING)
    (STRING COLON asm_operands)
    (STRING COLON asm_operands COLON asm_operands)
    (STRING COLON asm_operands COLON asm_operands COLON asm_clobbers)
    )
   (maybe_volatile
    ()
    (TYPE_QUAL)
    )
   (asm_operands
    ()
    (nonnull_asm_operands)
    )
   (nonnull_asm_operands
    (asm_operand)
    (nonnull_asm_operands COMMA asm_operand)
    )
   (asm_operand
    (STRING start_string_translation LPAREN expr RPAREN stop_string_translation)
    (LSBRA identifier RSBRA STRING start_string_translation LPAREN expr RPAREN stop_string_translation)
    )
   (asm_clobbers
    (STRING)
    (asm_clobbers COMMA STRING)
    )
   (stop_string_translation
    ()
    )
   (start_string_translation
    ()
    )
   (parmlist
    (maybe_attribute parmlist_1)
    )
   (parmlist_1
    (parmlist_2 RPAREN)
    (parms SEMICOLON maybe_attribute parmlist_1)
    (error RPAREN)
    )
   (parmlist_2
    ()
    (ELLIPSIS)
    (parms)
    (parms COMMA ELLIPSIS)
    )
   (parms
    (firstparm)
    (parms COMMA parm)
    )
   (parm
    (declspecs_ts setspecs parm_declarator maybe_attribute)
    (declspecs_ts setspecs notype_declarator maybe_attribute)
    (declspecs_ts setspecs absdcl_maybe_attribute)
    (declspecs_nots setspecs notype_declarator maybe_attribute)
    (declspecs_nots setspecs absdcl_maybe_attribute)
    )
   (firstparm
    (declspecs_ts_nosa setspecs_fp parm_declarator maybe_attribute)
    (declspecs_ts_nosa setspecs_fp notype_declarator maybe_attribute)
    (declspecs_ts_nosa setspecs_fp absdcl_maybe_attribute)
    (declspecs_nots_nosa setspecs_fp notype_declarator maybe_attribute)
    (declspecs_nots_nosa setspecs_fp absdcl_maybe_attribute)
    )
   (setspecs_fp
    (setspecs)
    )
   (parmlist_or_identifiers
    (maybe_attribute parmlist_or_identifiers_1)
    )
   (parmlist_or_identifiers_1
    (parmlist_1)
    (identifiers RPAREN)
    )
   (identifiers
    (IDENTIFIER)
    (identifiers COMMA IDENTIFIER)
    )
   (identifiers_or_typenames
    (identifier)
    (identifiers_or_typenames COMMA identifier)
    )
   (extension
    (EXTENSION)
    )

   ))

;; EOF
