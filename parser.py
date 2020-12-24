from lark import Lark

grammar = '''
    ?start : program
    
    %import common.WS_INLINE -> WHITESPACE
    
    program : function*
    
    IDENTIFIER : /[a-z]\w*/
    NUMBER : /(0|[1-9][0-9]*)/
    EOF : /[\n]/x
    
    assign : IDENTIFIER "=" (NUMBER | IDENTIFIER | sum)
    
    ?sum: product
        | sum "+" product   -> add
        | sum "-" product   -> sub

    ?product: atom
        | product "*" atom  -> mul
        | product "/" atom  -> div

    ?atom: NUMBER           -> number
         | "-" atom         -> neg
         | IDENTIFIER       -> var
         | "(" sum ")"
    
    _instruction : assign | _PASS
    
    _DEF : "def"
    _PASS : "pass"
    ARGUMENT : IDENTIFIER | NUMBER
    F_NAME : IDENTIFIER
    f_body : (" " ~ 4 _instruction)+
    f_arguments : "(" ( (ARGUMENT ",")+ ARGUMENT | ARGUMENT? ) ")"
    function : _DEF F_NAME f_arguments ":" f_body
    
    %ignore WHITESPACE
    %ignore EOF
'''

p = Lark(grammar, debug=True)

print(p.parse("""
def hello_world(a, b, c):
    a = ((10 + 20))*(-10)
    
def test(a, b, c):
    pass
""").pretty())
