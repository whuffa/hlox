{
module Parsle (parsle) where
import Prelude hiding (LT, GT, EQ, id)
import Check
import Data.Char
import Operators
import Tok
}

%name parsle
%tokentype { Token }
%error { parseError }

%monad { Check } { >>= } { return }

%token
    '+'     { Token (Symbol "+") _ }
    '-'     { Token (Symbol "-") _ }
    '*'     { Token (Symbol "*") _ }
    '/'     { Token (Symbol "/") _ }
    "not"   { Token (Symbol "!") _ }
    "and"   { Token (Symbol "&&") _ }
    "or"    { Token (Symbol "||") _ }
    "=="    { Token (Symbol "==") _ }
    "!="    { Token (Symbol "!=") _ }
    ">="    { Token (Symbol ">=") _ }
    "<="    { Token (Symbol "<=") _ }
    '>'     { Token (Symbol ">") _ }
    '<'     { Token (Symbol "<") _ }
    '('     { Token (Symbol "(") _ }
    ')'     { Token (Symbol ")") _ }
    literal { Token (Lit _) _ }

%left "or"
%left "and"
%left "==" "!="
%nonassoc '>' '<' ">=" "<="
%left '+' '-'
%left '*' '/'
%right "NEGATE" "not"



%%

Exp :: { Expr }
    : Exp '+' Exp    { Binary Add $1 $3 (getPos $2) }
    | Exp '-' Exp    { Binary Sub $1 $3 (getPos $2) }
    | Exp '*' Exp    { Binary Mul $1 $3 (getPos $2) }
    | Exp '/' Exp    { Binary Div $1 $3 (getPos $2) }
    | '-' Exp        %prec "NEGATE" { Unary Neg $2 (getPos $1) }
    | "not" Exp      { Unary Not $2 (getPos $1) }
    | '(' Exp ')'    { Group $2 (getPos $1) }
    | Exp "and" Exp  { Binary And $1 $3 (getPos $2) }
    | Exp "or" Exp   { Binary Or $1 $3 (getPos $2) }
    | Exp "==" Exp   { Binary EQ $1 $3 (getPos $2) }
    | Exp "!=" Exp   { Binary NE $1 $3 (getPos $2) }
    | Exp ">=" Exp   { Binary GE $1 $3 (getPos $2) }
    | Exp "<=" Exp   { Binary LE $1 $3 (getPos $2) }
    | Exp '>' Exp    { Binary GT $1 $3 (getPos $2) }
    | Exp '<' Exp    { Binary LT $1 $3 (getPos $2) }
    | literal        { let Token (Lit lit) pos = $1 in Litr lit pos }

{


parseError :: [Token] -> Check a
parseError [] = Error "Parse error: Unexpected end of input."
parseError ((Token tt pos):_) = Error $ "Unexpected token: " ++ (show tt) ++ stringPos pos


}