{
module Parsle (parsle) where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Operators
import Tok
}

%name parsle
%tokentype { TokenType }
%error { happyError }

%token
    '+'   { Symbol "+" }
    '-'   { Symbol "-" }
    '*'   { Symbol "*" }
    '/'   { Symbol "/" }
    '=='  { Symbol "==" }
    '!='  { Symbol "!=" }
    'not'   { Symbol "!" }
    '<'   { Symbol "<" }
    '<='  { Symbol "<=" }
    '>'   { Symbol ">" }
    '>='  { Symbol ">=" }
    '('   { Symbol "(" }
    ')'   { Symbol ")" }
    'or'  { Symbol "||"}
    'and' { Symbol "&&" }
    literal { Lit $$ }

%left '==' '!='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right "NEGATE" '!'



%%

Exp :: { Expr } 
    : Exp '+' Exp   { Binary Add $1 $3 }
    | Exp '-' Exp   { Binary Sub $1 $3 }
    | Exp '*' Exp   { Binary Mul $1 $3 }
    | Exp '/' Exp   { Binary Div $1 $3 }
    | '-' Exp       %prec "NEGATE" { Unary Neg $2 }
    | 'not' Exp     { Unary Not $2 } 
    | '(' Exp ')'   { Group $2 }
    | Exp 'and' Exp { Binary And $1 $3 }
    | Exp 'or' Exp  { Binary Or $1 $3 }
    | Exp '==' Exp  { Binary EQ $1 $3 }
    | Exp '!=' Exp  { Binary NE $1 $3 }
    | Exp '>=' Exp  { Binary GE $1 $3 }
    | Exp '<=' Exp  { Binary LE $1 $3 }
    | Exp '>' Exp   { Binary GT $1 $3 }
    | Exp '<' Exp   { Binary LT $1 $3 }
    | literal       { Litr $1 }

{


happyError :: [TokenType] -> a
happyError toks = error $ "Parse error at " ++ show toks
}