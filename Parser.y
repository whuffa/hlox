{
module Parser where
import Lexer
import Operators
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    fun { TokenKeyword "fun"}
    var { TokenKeyword "var"}
    if { TokenKeyword "if"}
    else { TokenKeyword "else"}
    true { TokenKeyword "true"}
    false { TokenKeyword "false"}
    class { TokenKeyword "class"}
    nil { TokenKeyword "nil"}
    return { TokenKeyword "return"}
    for { TokenKeyword "for" }
    print { TokenKeyword "print" }
    super { TokenKeyword "super" }
    while { TokenKeyword "while" }
    this { TokenKeyword "this" }

    '.' { Symbol "." }
    ',' { Symbol "," }
    '+' { Symbol "+" }
    '-' { Symbol "-" }
    '*' { Symbol "*" }
    '/' { Symbol "/" }
    '(' { Symbol "(" }
    ')' { Symbol ")" }
    '{' { Symbol "{" }
    '}' { Symbol "}" }
    ';' { Symbol ";" }
    '==' { Symbol "=="}
    '=' { Symbol "=" }
    '<=' { Symbol "<=" }
    '>=' { Symbol ">=" }
    '<' { Symbol "<" }
    '>' { Symbol ">" }
    '||' { Symbol "||" }
    '&&' { Symbol "&&" }
    '!=' { Symbol "!=" }
    '!' { Symbol "!"}
    digits {Digits $$}

%%

Expr : Expr2 ';' Expr
     | Expr2

Expr2 : 




