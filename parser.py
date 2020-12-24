from lark import Lark
from lark.indenter import Indenter

grammar = r"""
    single_input: _NEWLINE | simple_stmt | compound_stmt _NEWLINE
    file_input: (_NEWLINE | stmt)*
    eval_input: testlist _NEWLINE*
    
    decorator: "@" dotted_name [ "(" [arguments] ")" ] _NEWLINE
    decorators: decorator+
    decorated: decorators (classdef | funcdef | async_funcdef)
    
    async_funcdef: "async" funcdef
    funcdef: "def" NAME "(" parameters? ")" ["->" test] ":" suite
    
    parameters: paramvalue ("," paramvalue)* ["," "/"] ["," [starparams | kwparams]]
              | starparams
              | kwparams
    starparams: "*" typedparam? ("," paramvalue)* ["," kwparams]
    kwparams: "**" typedparam
    
    ?paramvalue: typedparam ["=" test]
    ?typedparam: NAME [":" test]
    
    varargslist: (vfpdef ["=" test] ("," vfpdef ["=" test])* ["," [ "*" [vfpdef] ("," vfpdef ["=" test])* ["," ["**" vfpdef [","]]] | "**" vfpdef [","]]]
      | "*" [vfpdef] ("," vfpdef ["=" test])* ["," ["**" vfpdef [","]]]
      | "**" vfpdef [","])
    
    vfpdef: NAME
    
    ?stmt: simple_stmt | compound_stmt
    ?simple_stmt: small_stmt (";" small_stmt)* [";"] _NEWLINE
    ?small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
    ?expr_stmt: testlist_star_expr (annassign | augassign (yield_expr|testlist)
             | ("=" (yield_expr|testlist_star_expr))*)
    annassign: ":" test ["=" test]
    ?testlist_star_expr: (test|star_expr) ("," (test|star_expr))* [","]
    !augassign: ("+=" | "-=" | "*=" | "@=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//=")
    // For normal and annotated assignments, additional restrictions enforced by the interpreter
    del_stmt: "del" exprlist
    pass_stmt: "pass"
    ?flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
    break_stmt: "break"
    continue_stmt: "continue"
    return_stmt: "return" [testlist]
    yield_stmt: yield_expr
    raise_stmt: "raise" [test ["from" test]]
    import_stmt: import_name | import_from
    import_name: "import" dotted_as_names
    // note below: the ("." | "...") is necessary because "..." is tokenized as ELLIPSIS
    import_from: "from" (dots? dotted_name | dots) "import" ("*" | "(" import_as_names ")" | import_as_names)
    !dots: "."+
    import_as_name: NAME ["as" NAME]
    dotted_as_name: dotted_name ["as" NAME]
    import_as_names: import_as_name ("," import_as_name)* [","]
    dotted_as_names: dotted_as_name ("," dotted_as_name)*
    dotted_name: NAME ("." NAME)*
    global_stmt: "global" NAME ("," NAME)*
    nonlocal_stmt: "nonlocal" NAME ("," NAME)*
    assert_stmt: "assert" test ["," test]
    
    compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt
    async_stmt: "async" (funcdef | with_stmt | for_stmt)
    if_stmt: "if" test ":" suite ("elif" test ":" suite)* ["else" ":" suite]
    while_stmt: "while" test ":" suite ["else" ":" suite]
    for_stmt: "for" exprlist "in" testlist ":" suite ["else" ":" suite]
    try_stmt: ("try" ":" suite ((except_clause ":" suite)+ ["else" ":" suite] ["finally" ":" suite] | "finally" ":" suite))
    with_stmt: "with" with_item ("," with_item)*  ":" suite
    with_item: test ["as" expr]
    // NB compile.c makes sure that the default except clause is last
    except_clause: "except" [test ["as" NAME]]
    suite: simple_stmt | _NEWLINE _INDENT stmt+ _DEDENT
    
    ?test: or_test ("if" or_test "else" test)? | lambdef
    ?test_nocond: or_test | lambdef_nocond
    lambdef: "lambda" [varargslist] ":" test
    lambdef_nocond: "lambda" [varargslist] ":" test_nocond
    ?or_test: and_test ("or" and_test)*
    ?and_test: not_test ("and" not_test)*
    ?not_test: "not" not_test -> not
             | comparison
    ?comparison: expr (_comp_op expr)*
    star_expr: "*" expr
    ?expr: xor_expr ("|" xor_expr)*
    ?xor_expr: and_expr ("^" and_expr)*
    ?and_expr: shift_expr ("&" shift_expr)*
    ?shift_expr: arith_expr (_shift_op arith_expr)*
    ?arith_expr: term (_add_op term)*
    ?term: factor (_mul_op factor)*
    ?factor: _factor_op factor | power
    
    !_factor_op: "+"|"-"|"~"
    !_add_op: "+"|"-"
    !_shift_op: "<<"|">>"
    !_mul_op: "*"|"@"|"/"|"%"|"//"
    // <> isn't actually a valid comparison operator in Python. It's here for the
    // sake of a __future__ import described in PEP 401 (which really works :-)
    !_comp_op: "<"|">"|"=="|">="|"<="|"<>"|"!="|"in"|"not" "in"|"is"|"is" "not"
    
    ?power: await_expr ("**" factor)?
    ?await_expr: AWAIT? atom_expr
    AWAIT: "await"
    
    ?atom_expr: atom_expr "(" [arguments] ")"      -> funccall
              | atom_expr "[" subscriptlist "]"  -> getitem
              | atom_expr "." NAME               -> getattr
              | atom
    
    ?atom: "(" [yield_expr|testlist_comp] ")" -> tuple
         | "[" [testlist_comp] "]"  -> list
         | "{" [dictorsetmaker] "}" -> dict
         | NAME -> var
         | number | string+
         | "(" test ")"
         | "..." -> ellipsis
         | "None"    -> const_none
         | "True"    -> const_true
         | "False"   -> const_false
    
    ?testlist_comp: (test|star_expr) [comp_for | ("," (test|star_expr))+ [","] | ","]
    subscriptlist: subscript ("," subscript)* [","]
    subscript: test | [test] ":" [test] [sliceop]
    sliceop: ":" [test]
    exprlist: (expr|star_expr) ("," (expr|star_expr))* [","]
    testlist: test ("," test)* [","]
    dictorsetmaker: ( ((test ":" test | "**" expr) (comp_for | ("," (test ":" test | "**" expr))* [","])) | ((test | star_expr) (comp_for | ("," (test | star_expr))* [","])) )
    
    classdef: "class" NAME ["(" [arguments] ")"] ":" suite
    
    arguments: argvalue ("," argvalue)*  ("," [ starargs | kwargs])?
             | starargs
             | kwargs
             | test comp_for
    
    starargs: "*" test ("," "*" test)* ("," argvalue)* ["," kwargs]
    kwargs: "**" test
    
    ?argvalue: test ("=" test)?
    
    
    
    comp_iter: comp_for | comp_if | async_for
    async_for: "async" "for" exprlist "in" or_test [comp_iter]
    comp_for: "for" exprlist "in" or_test [comp_iter]
    comp_if: "if" test_nocond [comp_iter]
    
    // not used in grammar, but may appear in "node" passed from Parser to Compiler
    encoding_decl: NAME
    
    yield_expr: "yield" [yield_arg]
    yield_arg: "from" test | testlist
    
    
    number: DEC_NUMBER | HEX_NUMBER | BIN_NUMBER | OCT_NUMBER | FLOAT_NUMBER | IMAG_NUMBER
    string: STRING | LONG_STRING
    
    // Import terminals from standard library (grammars/python.lark)
    %import python (NAME, COMMENT, STRING, LONG_STRING)
    %import python (DEC_NUMBER, HEX_NUMBER, OCT_NUMBER, BIN_NUMBER, FLOAT_NUMBER, IMAG_NUMBER)
    
    // Other terminals
    
    _NEWLINE: ( /\r?\n[\t ]*/ | COMMENT )+
    
    %ignore /[\t \f]+/  // WS
    %ignore /\\[\t \f]*\r?\n/   // LINE_CONT
    %ignore COMMENT
    %declare _INDENT _DEDENT
"""

grammar = '''
    ?start : program
 
 
    program : _EOF* function*
 
    IDENTIFIER : /(?!(if|else|def|pass|return|while)\s)[a-z]\w*/
    NUMBER : /(0|[1-9][0-9]*)/
    _EOF : /[\n]/x
 
    
    assign : IDENTIFIER "=" expression
 
 
    ?expression : disj
 
    ?disj : conj
        | conj "||" disj -> disj
 
    ?conj : not
        | not "&&" conj -> conj
 
    ?not : comparation
        | "--" comparation -> not
 
    ?comparation : sum
        | comparation "==" comparation -> eq
        | comparation "/=" comparation -> neq
        | comparation "<" comparation -> lt
        | comparation "<=" comparation -> lte
        | comparation ">" comparation -> gt
        | comparation ">=" comparation -> gte
 
    ?sum : product
        | sum "+" product -> add
        | sum "-" product -> sub

    ?product : power
        | product "*" power -> mul
        | product "/" power -> div
 
    ?power : atom
        | atom "**" power -> power

    ?atom : NUMBER
        | "-" atom -> neg
        | IDENTIFIER
        | "(" expression ")"
  
 
    _IF.2 : "if"
    _ELSE : "else"
    condition : expression
    if_body : (_INDENT _instruction _DEDENT)+
    else_body : (_INDENT _instruction _DEDENT)+
    cond_statement : _IF condition ":" _EOF if_body _ELSE ":" _EOF else_body
 
 
    _WHILE : "while"
    while_body : (_INDENT _instruction _DEDENT)+
    while_statement : _WHILE condition ":" _EOF while_body
 
 
    _instruction : assign | return | _PASS
 
    _DEF : "def"
    _PASS : "pass"
    _RETURN : "return"
    return : _RETURN " " expression
    ARGUMENT : IDENTIFIER | NUMBER
    F_NAME : IDENTIFIER
    f_body : (_INDENT (_instruction | return | cond_statement | while_statement) _DEDENT)+
    f_arguments : "(" ( (ARGUMENT ",")+ ARGUMENT | ARGUMENT? ) ")"
    function : _DEF F_NAME f_arguments ":" _EOF f_body
    
    %import common.WS_INLINE
    %declare _INDENT _DEDENT
    %ignore WS_INLINE
'''


test_tree = """
a
    b
    c
        d
        e
    f
        g
"""

class PythonIndenter(Indenter):
    NL_type = '_NEWLINE'
    OPEN_PAREN_types = ['LPAR', 'LSQB', 'LBRACE']
    CLOSE_PAREN_types = ['RPAR', 'RSQB', 'RBRACE']
    INDENT_type = '_INDENT'
    DEDENT_type = '_DEDENT'
    tab_len = 8


p = Lark(tree_grammar, debug=True, parser='lalr', postlex=PythonIndenter())


print(p.parse("""
def hello_world():
    pass
""").pretty())
