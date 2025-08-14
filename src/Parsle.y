{
module Parsle (parsle) where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Operators
import Tok
}

%name parsle
%tokentype { Token }
%error { happyError }

%token
    '+'   { Symbol "+" }
    '-'   { Symbol "-" }
    '*'   { Symbol "*" }
    '/'   { Symbol "/" }
    '=='  { Symbol "==" }
    '!='  { Symbol "!=" }
    '!'   { Symbol "!" }
    '<'   { Symbol "<" }
    '<='  { Symbol "<=" }
    '>'   { Symbol ">" }
    '>='  { Symbol ">=" }
    '('   { Symbol "(" }
    ')'   { Symbol ")" }
    "or"  { TokenKeyword "or" }
    "and" { TokenKeyword "and" }
    "not" { TokenKeyword "not" }
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
    | '(' Exp ')'   { Group $2 }
    | literal       { Litr $1 }

{


happyError :: [Token] -> a
happyError toks = error $ "Parse error at " ++ show toks
}