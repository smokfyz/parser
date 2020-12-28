import sys

from lark import Lark
from lark.indenter import Indenter
from lark.tree import pydot__tree_to_png

try:
    input_file = sys.argv[1]
    output_file = sys.argv[2]
except IndexError:
    print("Введите название входного и выходного файла.")
    exit(0)

grammar = r"""
    single_input: _NEWLINE | simple_stmt | compound_stmt _NEWLINE
    start: (_NEWLINE | stmt)*
    
    funcdef: "def" NAME "(" parameters? ")" ":" suite
    
    parameters: paramvalue ("," paramvalue)*
    
    ?paramvalue: NAME
    
    ?stmt: simple_stmt | compound_stmt
    ?simple_stmt: small_stmt (";" small_stmt)* [";"] _NEWLINE
    ?small_stmt: (expr_stmt | pass_stmt | flow_stmt)
    ?expr_stmt: test (annassign | ("=" (test))*)
    annassign: ":" test ["=" test]

    pass_stmt: "pass"
    ?flow_stmt: return_stmt
    return_stmt: "return" test
    
    ?compound_stmt: if_stmt | while_stmt | funcdef
    if_stmt: "if" test ":" if_suite ["else" ":" else_suite]
    while_stmt: "while" test ":" suite ["else" ":" suite]
    suite: simple_stmt | _NEWLINE _INDENT stmt+ _DEDENT
    if_suite: simple_stmt | _NEWLINE _INDENT stmt+ _DEDENT
    else_suite: simple_stmt | _NEWLINE _INDENT stmt+ _DEDENT
    
    ?test: expr
    ?expr: or_expr
    ?or_expr: (and_expr "||")* and_expr
    ?and_expr: (comparison "&&")* comparison
    ?comparison: not_expr (_comp_op not_expr)*
    ?not_expr: "--" add_expr -> not_expr
        | add_expr
    ?add_expr: mul_expr (_add_op mul_expr)*
    ?mul_expr: factor (_mul_op factor)*
    ?factor: _factor_op factor | power
    
    !_factor_op: "-"
    !_add_op: "+"|"-"
    !_mul_op: "*"|"/"
    
    MUL: "*"
    DIV: "/"
    ADD: "+"
    SUB: "-"
    LT: "<"
    LTE: "<="
    GT: ">"
    GTE: ">="
    NEQ: "/="
    EQ: "=="

    !_comp_op: "<"|">"|"=="|">="|"<="|"/="
    
    ?power: atom_expr ("**" factor)?
    
    ?atom_expr: atom_expr "(" [arguments] ")" -> funccall
              | atom
    
    ?atom: NAME -> var
         | NUMBER -> number
         | "(" test ")"
            
    arguments: argvalue ("," argvalue)*
    
    ?argvalue: test?
    
    NUMBER: /(0|[1-9][0-9]*)/
    NAME: /(?!(if|else|def|pass|return|while)\b)[a-z]\w*/
        
    _NEWLINE: ( /\r?\n[\t ]*/)+
    
    %ignore /[\t \f]+/  // WS
    %ignore /\\[\t \f]*\r?\n/   // LINE_CONT
    %declare _INDENT _DEDENT
"""


class PythonIndenter(Indenter):
    NL_type = '_NEWLINE'
    OPEN_PAREN_types = ['LPAR', 'LSQB', 'LBRACE']
    CLOSE_PAREN_types = ['RPAR', 'RSQB', 'RBRACE']
    INDENT_type = '_INDENT'
    DEDENT_type = '_DEDENT'
    tab_len = 4


p = Lark(grammar, parser='lalr', postlex=PythonIndenter())

if __name__ == '__main__':
    with open(input_file, "r") as file:
        try:
            code = file.read()
            tree = p.parse(code)
            pydot__tree_to_png(tree, output_file)
        except Exception as e:
            print(f"Parse error! {e}")
