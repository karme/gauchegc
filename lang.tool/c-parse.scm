#|
This file (c-parse.scm) is converted from c-parse.in 
by Shigenobu Kimura <skimu@mac.com>,  7/11/2004.

/* YACC parser for C syntax and for Objective C.  -*-c-*-
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */
|#

;;(use lalr)

(define c-parse
  (lalr-parser
   (output:    c-parse "c-parse.yy.scm-orig")
   (out-table: "c-parse.out")
   (expect: 13) ; shift/reduce conflicts, and no reduce/reduce conflicts.
   (ID  SEMICOLON                       ;
        ;; For Scheme,  LPAREN LSBRA  are defined blow
        ;; COMMA=,  LCBRA={  RCBRA=} LSBRA=[  RSBRA=]
        ;; LPAREN=( RPAREN=) PIPE=| DOT=.
        COMMA RSBRA LCBRA RCBRA RPAREN ; DOT 
        
        ~ !

        ;; All identifiers that are declared typedefs in the current block.
        ;; In some contexts, they are treated just like IDENTIFIER,
        ;;   but they can also serve as typespecs in declarations.
        IDENTIFIER
        TYPENAME
        ;; Reserved words that specify storage class.
        ;; yylval contains an IDENTIFIER_NODE which indicates which one.
        SCSPEC                     ; Storage class other than static.
        STATIC                     ; Static storage class.
        ;; Reserved words that specify type.
        ;; yylval contains an IDENTIFIER_NODE which indicates which one.
        TYPESPEC
        ;; Reserved words that qualify type: "const", "volatile", or "restrict".
        ;; yylval contains an IDENTIFIER_NODE which indicates which one.  */
        TYPE_QUAL

        ;; Character or numeric constants.
        ;; yylval is the node for the constant.
        CONSTANT

        ;; String constants in raw form.
        ;; yylval is a STRING_CST node.
        STRING

        ;; "...", used for functions with variable arglists. 
        ELLIPSIS

        ;; the reserved words 
        ;; SCO include files test "ASM", so use something else. 
        SIZEOF ENUM STRUCT UNION WHILE DO FOR SWITCH CASE DEFAULT ; IF ELSE 
        BREAK CONTINUE RETURN GOTO ASM_KEYWORD TYPEOF ALIGNOF
        ATTRIBUTE EXTENSION LABEL
        REALPART IMAGPART VA_ARG CHOOSE_EXPR TYPES_COMPATIBLE_P
        PTR_VALUE PTR_BASE PTR_EXTENT
        FUNC_NAME OFFSETOF

        ;; Add precedence rules to solve dangling else s/r conflict 
        (nonassoc: IF)
        (nonassoc: ELSE)

        ;; Define the operator tokens and their precedences.
        ;; The value is an integer because, if used, it is the tree code
        ;; to use in the expression made from the operator.  */

        (right:  ASSIGN =)
        (right:  ? COLON)
        (left:  OROR)
        (left:  ANDAND)
        (left:  OR)
        (left:  ^)
        (left:  &)
        (left:  EQCOMPARE)
        (left:  ARITHCOMPARE)
        (left:  LSHIFT RSHIFT)
        (left:  + -)
        (left:  * / %)
        (right: UNARY PLUSPLUS MINUSMINUS)
        (left: HYPERUNARY)
        (left: POINTSAT DOT LPAREN LSBRA)

        ;; The Objective-C keywords.  These are included in C and in
        ;; Objective C, so that the token codes are the same in both.
        AT_INTERFACE AT_IMPLEMENTATION AT_END AT_SELECTOR AT_DEFS AT_ENCODE
        CLASSNAME AT_PUBLIC AT_PRIVATE AT_PROTECTED AT_PROTOCOL 
        OBJECTNAME AT_CLASS AT_ALIAS
        AT_THROW AT_TRY AT_CATCH AT_FINALLY AT_SYNCHRONIZED
        OBJC_STRING
        )
   ;; rules
   (program 
    () 
    (extdefs)
    )

   ;; the reason for the strange actions in this rule
   ;; is so that notype_initdecls when reached via datadef
   ;; can find a valid list of type and sc specs in $0.

   (extdefs
    (extdef)                  : (ppvec $1)
    (extdefs extdef)          : (ppvec $2)
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
    (declspecs_ts setspecs declarator
		  old_style_parm_decls save_location
		  compstmt_or_error)
    ;;(declspecs_ts setspecs declarator error)
    (declspecs_nots setspecs notype_declarator
		    old_style_parm_decls save_location
		    compstmt_or_error)
    ;;(declspecs_nots setspecs notype_declarator error)
    (setspecs notype_declarator
	      old_style_parm_decls save_location
	      compstmt_or_error)
    ;;(error setspecs notype_declarator)
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
    ;; __extension__ turns off -pedantic for following primary.  */
    (extension cast_expr (prec: UNARY))
    (unop cast_expr  (prec: UNARY))
    ;; Refer to the address of a label as a pointer.
    (ANDAND identifier)
    (sizeof unary_expr  (prec: UNARY))
    (sizeof LPAREN typename RPAREN  (prec: HYPERUNARY))
    (alignof unary_expr  (prec: UNARY))
    (alignof LPAREN typename RPAREN  (prec: HYPERUNARY))
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
    (LPAREN typename RPAREN cast_expr  (prec: UNARY))
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
    (expr_no_commas OROR	  expr_no_commas)
    (expr_no_commas ? expr COLON expr_no_commas)
    (expr_no_commas ?  COLON expr_no_commas)
    (expr_no_commas = expr_no_commas)
    (expr_no_commas ASSIGN expr_no_commas)
    )

   (primary
    (IDENTIFIER)
    (CONSTANT)
    (STRING)
    (FUNC_NAME)
    (LPAREN typename RPAREN LCBRA
	    initlist_maybe_comma RCBRA  (prec: UNARY))
    (LPAREN expr RPAREN)
    ;;(LPAREN error RPAREN)
    (compstmt_primary_start compstmt_nostart RPAREN)
    ;;(compstmt_primary_start error RPAREN)
    (primary LPAREN exprlist RPAREN   (prec: DOT))
    (VA_ARG LPAREN expr_no_commas COMMA typename RPAREN)
    (OFFSETOF LPAREN typename COMMA offsetof_member_designator RPAREN)
    ;;(OFFSETOF LPAREN error RPAREN)
    (CHOOSE_EXPR LPAREN expr_no_commas COMMA expr_no_commas COMMA
		 expr_no_commas RPAREN)
    ;;(CHOOSE_EXPR LPAREN error RPAREN)
    (TYPES_COMPATIBLE_P LPAREN typename COMMA typename RPAREN)
    ;;(TYPES_COMPATIBLE_P LPAREN error RPAREN)
    (primary LSBRA expr RSBRA   (prec: DOT))
    (primary DOT identifier)
    (primary POINTSAT identifier)
    (primary PLUSPLUS)
    (primary MINUSMINUS)
    )


   ;; This is the second argument to __builtin_offsetof.  We must have one
   ;; identifier, and beyond that we want to accept sub structure and sub
   ;; array references.  We return tree list where each element has
   ;; PURPOSE set for component refs or VALUE set for array refs.  We'll
   ;;  turn this into something real inside build_offsetof.  */
   
   (offsetof_member_designator 
    (identifier)
    (offsetof_member_designator DOT identifier)
    (offsetof_member_designator LSBRA expr RSBRA)
    )

   (old_style_parm_decls
    ()
    (datadecls)
    )

   ;; The following are analogous to lineno_decl, decls and decl
   ;; except that they do not allow nested functions.
   ;; They are used for old-style parm decls.
   (lineno_datadecl
    (save_location datadecl)
    )

   (datadecls 
    (lineno_datadecl)
    (errstmt)
    (datadecls lineno_datadecl)
    (lineno_datadecl errstmt)
    )

   ;; We don't allow prefix attributes here because they cause reduce/reduce
   ;; conflicts: we can't know whether we're parsing a function decl with
   ;; attribute suffix, or function defn with attribute prefix on first old
   ;; style parm.
   (datadecl 
    (declspecs_ts_nosa setspecs initdecls SEMICOLON)
    (declspecs_nots_nosa setspecs notype_initdecls SEMICOLON)
    (declspecs_ts_nosa SEMICOLON)
    (declspecs_nots_nosa SEMICOLON)
    )

   ;; This combination which saves a lineno before a decl
   ;; is the normal thing to use, rather than decl itself.
   ;; This is to avoid shift/reduce conflicts in contexts
   ;; where statement labels are allowed.  */
   (lineno_decl 
    (save_location decl)
    )

   ;; records the type and storage class specs to use for processing
   ;; the declarators that follow.
   ;; Maintains a stack of outer-level values of current_declspecs,
   ;; for the sake of parm declarations nested in function declarators.
   (setspecs 
    () 
    )

   ;; Possibly attributes after a comma, which should reset all_prefix_attributes
   ;; to prefix_attributes with these ones chained on the front.
   (maybe_resetattrs (maybe_attribute))

   (decl 
    (declspecs_ts setspecs initdecls SEMICOLON)
    (declspecs_nots setspecs notype_initdecls SEMICOLON)
    (declspecs_ts setspecs nested_function)
    (declspecs_nots setspecs notype_nested_function)
    (declspecs SEMICOLON)
    (extension decl)
    )


   ;;  A list of declaration specifiers.  These are:
   ;;
   ;;  - Storage class specifiers (scspec), which for GCC currently includes
   ;;  function specifiers ("inline").
   ;;
   ;;  - Type specifiers (typespec_*).
   ;;
   ;;  - Type qualifiers (TYPE_QUAL).
   ;;
   ;;  - Attribute specifier lists (attributes).
   ;;
   ;;  These are stored as a TREE_LISTthe head of the list is the last
   ;;  item in the specifier list.  Each entry in the list has either a
   ;;  TREE_PURPOSE that is an attribute specifier list, or a TREE_VALUE that
   ;;  is a single other specifier or qualifierand a TREE_CHAIN that is the
   ;;  rest of the list.  TREE_STATIC is set on the list if something other
   ;;  than a storage class specifier or attribute has been seenthis is used
   ;;  to warn for the obsolescent usage of storage class specifiers other than
   ;;  at the start of the list.  (Doing this properly would require function
   ;;  specifiers to be handled separately from storage class specifiers.)
   ;;
   ;;  The various cases below are classified according to:
   ;;
   ;;  (a) Whether a storage class specifier is included or notsome
   ;;  places in the grammar disallow storage class specifiers (_sc or _nosc).
   ;;
   ;;  (b) Whether a type specifier has been seenafter a type specifier,
   ;;  a typedef name is an identifier to redeclare (_ts or _nots).
   ;;
   ;;  (c) Whether the list starts with an attributein certain places,
   ;;  the grammar requires specifiers that don't start with an attribute
   ;;  (_sa or _nosa).
   ;;
   ;;  (d) Whether the list ends with an attribute (or a specifier such that
   ;;  any following attribute would have been parsed as part of that specifier)
   ;;  this avoids shift-reduce conflicts in the parsing of attributes
   ;;  (_ea or _noea).
   ;;
   ;;  TODO:
   ;;
   ;;  (i) Distinguish between function specifiers and storage class specifiers,
   ;;  at least for the purpose of warnings about obsolescent usage.
   ;;
   ;;  (ii) Halve the number of productions here by eliminating the _sc/_nosc
   ;;  distinction and instead checking where required that storage class
   ;;  specifiers aren't present. 
   
   ;; Declspecs which contain at least one type specifier or typedef name.
   ;; (Just `const' or `volatile' is not enough.)
   ;; A typedef'd name following these is taken as a name to be declared.
   ;; Declspecs have a non-NULL TREE_VALUE, attributes do not.

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

   ;; Particular useful classes of declspecs.
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

   ;; A (possibly empty) sequence of type qualifiers and attributes.
   (maybe_type_quals_attrs 
    ()
    (declspecs_nosc_nots)
    )

   ;; A type specifier (but not a type qualifier).
   ;; Once we have seen one of these in a declaration,
   ;; if a typedef name appears then it is being redeclared.
   ;;  The _reserved versions start with a reserved word and may appear anywhere
   ;; in the declaration specifiers; the _nonreserved versions may only
   ;; appear before any other type specifiers, and after that are (if names)
   ;; being redeclared.
   ;;  FIXME: should the _nonreserved version be restricted to names being
   ;; redeclared only?  The other entries there relate only the GNU extensions
   ;; and Objective C, and are historically parsed thus, and don't make sense
   ;; after other type specifiers, but it might be cleaner to count them as
   ;; _reserved.
   ;;  _attr means: specifiers that either end with attributes,
   ;; or are such that any following attributes would
   ;; be parsed as part of the specifier.
   ;;  _nonattr: specifiers. 
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
    (structsp_attr))

   (typespec_nonreserved_nonattr 
    (TYPENAME)
    (typeof LPAREN expr RPAREN)
    (typeof LPAREN typename RPAREN)
    )

   ;; typespec_nonreserved_attr does not exist.

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
    ;; Note how the declaration of the variable is in effect while its init is parsed!
    (declarator maybeasm maybe_attribute)
    )

   (notype_initdcl 
    (notype_declarator maybeasm maybe_attribute = init)
    ;; Note how the declaration of the variable is in effect while its init is parsed!
    (notype_declarator maybeasm maybe_attribute)
    )
		
                                        ;
   ;; the * rules are dummies to accept the Apollo extended syntax
   ;; so that the header files compile.
   (maybe_attribute 
    ()
    (attributes)
    )

   (attributes 
    (attribute)
    (attributes attribute)
    )

   (attribute 
    (ATTRIBUTE stop_string_translation
	       LPAREN LPAREN attribute_list RPAREN RPAREN start_string_translation)
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

   ;; This still leaves out most reserved keywords,
   ;; shouldn't we include them?
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

   ;; Initializers.  `init' is the entry point. 

   (init 
    (expr_no_commas)
    (LCBRA initlist_maybe_comma RCBRA)
    ;;(error)
    )

   ;; `initlist_maybe_comma' is the guts of an initializer in braces.
   (initlist_maybe_comma 
    ()
    (initlist1 maybecomma)
    )

   (initlist1 
    (initelt)
    (initlist1 COMMA initelt)
    )

   ;; `initelt' is a single element of an initializer.
   ;; It may use braces. 
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
    (declarator old_style_parm_decls  save_location 
     ;; This (compstmt) used to use compstmt_or_error.  That caused a bug with
     ;; input `f(g) int g ', where the use of YYERROR1 above caused
     ;; an error which then was handled by compstmt_or_error.  There
     ;; followed a repeated execution of that same rule, which called
     ;; YYERROR1 again, and so on. 
     compstmt)
    )

   ;; This (compstmt) used to use compstmt_or_error.  That caused a bug with
   ;; input `f(g) int g ', where the use of YYERROR1 above caused
   ;; an error which then was handled by compstmt_or_error.  There
   ;; followed a repeated execution of that same rule, which called
   ;; YYERROR1 again, and so on. 
   (notype_nested_function 
    (notype_declarator old_style_parm_decls save_location 
     ;; This (compstmt) used to use compstmt_or_error.  That caused a bug with
     ;; input `f(g) int g ', where the use of YYERROR1 above caused
     ;; an error which then was handled by compstmt_or_error.  There
     ;; followed a repeated execution of that same rule, which called
     ;; YYERROR1 again, and so on. 
     compstmt))


   ;; Any kind of declarator (thus, all declarators allowed
   ;; after an explicit typespec).  */
   (declarator 
    (after_type_declarator)
    (notype_declarator)
    )

   ;; A declarator that is allowed only after an explicit typespec.
   (after_type_declarator 
    (LPAREN maybe_attribute after_type_declarator RPAREN)
    (after_type_declarator LPAREN parmlist_or_identifiers  (prec: DOT))
    (after_type_declarator array_declarator  (prec: DOT))
    (* maybe_type_quals_attrs after_type_declarator  (prec: UNARY))
    (TYPENAME)
    )

   ;;  Kinds of declarator that can appear in a parameter list
   ;; in addition to notype_declarator.  This is like after_type_declarator
   ;; but does not allow a typedef name in parentheses as an identifier
   ;; (because it would conflict with a function with that typedef as arg).
   (parm_declarator 
    (parm_declarator_starttypename)
    (parm_declarator_nostarttypename)
    )

   (parm_declarator_starttypename 
    (parm_declarator_starttypename 
     LPAREN parmlist_or_identifiers  (prec: DOT))
    (parm_declarator_starttypename 
     array_declarator  (prec: DOT))
    (TYPENAME)
    )

   (parm_declarator_nostarttypename 
    (parm_declarator_nostarttypename 
     LPAREN parmlist_or_identifiers  (prec: DOT))
    (parm_declarator_nostarttypename array_declarator  
				     (prec: DOT))
    (* maybe_type_quals_attrs 
       parm_declarator_starttypename  (prec: UNARY))
    (* maybe_type_quals_attrs
       parm_declarator_nostarttypename  (prec: UNARY))
    (LPAREN maybe_attribute
	    parm_declarator_nostarttypename RPAREN)
    )		

   ;; A declarator allowed whether or not there has been
   ;; an explicit typespec.  These cannot redeclare a typedef-name.
   (notype_declarator 
    (notype_declarator LPAREN parmlist_or_identifiers  (prec: DOT))
    (LPAREN maybe_attribute notype_declarator RPAREN)
    (* maybe_type_quals_attrs notype_declarator  (prec: UNARY))
    (notype_declarator array_declarator  (prec: DOT))
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


   ;; structsp_attr: struct/union/enum specifiers that either
   ;; end with attributes, or are such that any following attributes would
   ;; be parsed as part of the struct/union/enum specifier.
   ;;
   ;; structsp_nonattr: other struct/union/enum specifiers.

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


   ;; We chain the components in reverse order.  They are put in forward
   ;; order in structsp_attr.
   ;; 
   ;; Note that component_declarator returns single decls, so components
   ;; and components_notype can use TREE_CHAIN directly, wheras components
   ;; and components_notype return lists (of comma separated decls), so
   ;; component_decl_list and component_decl_list2 must use chainon.
   ;; 
   ;; The theory behind all this is that there will be more semicolon
   ;; separated fields than comma separated fields, and so we'll be
   ;; minimizing the number of node traversals required by chainon. 

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

   ;; We chain the enumerators in reverse order.
   ;; They are put in forward order in structsp_attr.
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

   ;; an absolute declarator
   (absdcl 
    ()
    (absdcl1)
    )

   ;; absdcl maybe_attribute, but not just attributes 
   (absdcl_maybe_attribute 
    () 
    (absdcl1)
    (absdcl1_noea attributes)
    )

   ;; a nonempty absolute declarator
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

   ;; The [...] part of a declarator for an array type.  */

   (array_declarator 
    (LSBRA maybe_type_quals_attrs expr_no_commas RSBRA)
    (LSBRA maybe_type_quals_attrs RSBRA)
    (LSBRA maybe_type_quals_attrs * RSBRA)
    (LSBRA STATIC maybe_type_quals_attrs expr_no_commas RSBRA)
    ;;  declspecs_nosc_nots is a synonym for type_quals_attrs. 
    (LSBRA declspecs_nosc_nots STATIC expr_no_commas RSBRA)
    )

   ;; A nonempty series of declarations and statements (possibly followed by
   ;; some labels) that can form the body of a compound statement.
   ;; NOTE: we don't allow labels on declarations; this might seem like a
   ;; natural extension, but there would be a conflict between attributes
   ;; on the label and prefix attributes on the declaration.  */

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

   ;; Start and end blocks created for the new scopes of C99.
   (c99_block_start () )

   ;; Read zero or more forward-declarations for labels
   ;; that nested functions can jump to.
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

   ;; This is the body of a function definition.
   ;; It causes syntax errors to ignore to the next openbrace.
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

   (compstmt (compstmt_start compstmt_nostart)
             )

   ;; The forced readahead in here is because we might be at the end of a
   ;; line, and the line and file won't be bumped until yylex absorbs the
   ;; first token on the next line.  */

   (save_location 
    ()
    )

   (lineno_labels 
    ()
    (lineno_labels lineno_label)
    )

   ;; A labeled statement.  In C99 it also generates an implicit block.
   (c99_block_lineno_labeled_stmt 
    (c99_block_start lineno_labels lineno_stmt) )

   (lineno_stmt 
    (save_location stmt)
    )

   (lineno_label 
    (save_location label)
    )

   (condition 
    (save_location expr)
    )

   ;; Implement -Wparenthesis by special casing IF statement directly nested
   ;; within IF statement.  This requires some amount of duplication of the
   ;; productions under c99_block_lineno_labeled_stmt in order to work out.
   ;; But it's still likely more maintainable than lots of state outside the
   ;; parser...
   (if_statement_1 
    (c99_block_start lineno_labels if_statement)
    )

   (if_statement_2 
    (c99_block_start lineno_labels SEMICOLON)
    (c99_block_lineno_labeled_stmt)
    )

   (if_statement 
    (IF c99_block_start save_location LPAREN condition RPAREN
	if_statement_1 ELSE if_statement_2)
    (IF c99_block_start save_location LPAREN condition RPAREN
	if_statement_2 ELSE if_statement_2)
    (IF c99_block_start save_location LPAREN condition RPAREN
	if_statement_1				(prec: IF))
    (IF c99_block_start save_location LPAREN condition RPAREN
	if_statement_2				(prec: IF))
    )

   (start_break 
    ()
    )

   (start_continue 
    ()
    )

   (while_statement 
    (WHILE c99_block_start save_location LPAREN condition RPAREN
	   start_break start_continue c99_block_lineno_labeled_stmt)
    )
		
   (do_statement 
    (DO c99_block_start save_location start_break start_continue
	c99_block_lineno_labeled_stmt WHILE 
	LPAREN condition RPAREN SEMICOLON)
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
    (FOR c99_block_start LPAREN for_init_stmt
	 save_location for_cond_expr SEMICOLON for_incr_expr RPAREN
	 start_break start_continue c99_block_lineno_labeled_stmt)
    )

   (switch_statement 
    (SWITCH c99_block_start LPAREN expr RPAREN
	    start_break c99_block_lineno_labeled_stmt)
    )

   ;; Parse a single real statement, not including any labels or compounds.
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


   ;; Parse a single or compound real statement, not including any labels.
   (stmt 
    (compstmt)
    (stmt_nocomp)
    )

   ;; Any kind of label, including jump labels and case labels.
   ;; ANSI C accepts labels only before statements, but we allow them
   ;; also at the end of a compound statement.

   (label 
    (CASE expr_no_commas COLON)
    (CASE expr_no_commas ELLIPSIS expr_no_commas COLON)
    (DEFAULT COLON)
    (identifier save_location COLON maybe_attribute)
    )

   ;; Asm expressions and statements */

   ;; simple_asm_expr is used in restricted contexts, where a full
   ;; expression with inputs and outputs does not make sense.
   (simple_asm_expr 
    (ASM_KEYWORD stop_string_translation
		 LPAREN STRING RPAREN start_string_translation)
    )
   
   ;; maybeasm: used for assembly names for declarations
   (maybeasm 
    ()
    (simple_asm_expr)
    )

   ;; asmdef: asm() outside a function body.
   (asmdef 
    (simple_asm_expr SEMICOLON)
    ;;(ASM_KEYWORD error start_string_translation SEMICOLON)
    )

   ;; Full-blown asm statement with inputs, outputs, clobbers, and
   ;; volatile tag allowed. 
   (asm_stmt 
    (ASM_KEYWORD maybe_volatile stop_string_translation
		 LPAREN asm_argument RPAREN start_string_translation SEMICOLON)
    )

   (asm_argument  
    (STRING)				; no operands
    (STRING COLON asm_operands)		; output operands
    (STRING COLON asm_operands COLON asm_operands) ;  output and input operands
    (STRING COLON asm_operands 
	    COLON asm_operands COLON asm_clobbers) ; output and input operands and clobbers
    )

   ;; Either 'volatile' or nothing.  First thing in an `asm' statement.
   (maybe_volatile 
    ()
    (TYPE_QUAL)
    )

   ;; These are the operands other than the first string and colon
   ;; in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */
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
    (LSBRA identifier RSBRA STRING start_string_translation
	   LPAREN expr RPAREN stop_string_translation)
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

   ;; This is what appears inside the parens in a function declarator.
   ;; Its value is a list of ..._TYPE nodes.  Attributes must appear here
   ;; to avoid a conflict with their appearance after an open parenthesis
   ;; in an abstract declarator, as in
   ;; "void bar (int (__attribute__((__mode__(SI))) int foo));".  */
   (parmlist 
    (maybe_attribute parmlist_1)
    )

   (parmlist_1 
    (parmlist_2 RPAREN)
    (parms SEMICOLON
	   maybe_attribute
	   parmlist_1)
    ;;(error RPAREN)
    )

   ;; This is what appears inside the parens in a function declarator.
   ;; Is value is represented in the format that grokdeclarator expects.  */
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

   ;; A single parameter declaration or parameter type name,
   ;; as found in a parmlist.
   (parm 
    (declspecs_ts setspecs parm_declarator maybe_attribute)
    (declspecs_ts setspecs notype_declarator maybe_attribute)
    (declspecs_ts setspecs absdcl_maybe_attribute)
    (declspecs_nots setspecs notype_declarator maybe_attribute)
    (declspecs_nots setspecs absdcl_maybe_attribute)
    )

   ;; The first parm, which must suck attributes from off the top of the parser
   ;; stack.
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

   ;; This is used in a function definition
   ;; where either a parmlist or an identifier list is ok.
   ;; Its value is a list of ..._TYPE nodes or a list of identifiers.

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

   ;;  A nonempty list of identifiers, including typenames. 
   (identifiers_or_typenames 
    (identifier)
    (identifiers_or_typenames COMMA identifier)
    )

   (extension 
    (EXTENSION)
    )
   ))
