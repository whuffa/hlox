{
module Parsle (parsle) where
import Prelude hiding (LT, GT, EQ, id)
import Check(Check(..))
import Data.Char
import Operators
import Tok
}

%name parsle
%tokentype { Token }
%error { parseError }

%monad { Check String } { >>= } { return }

%token
    ';'     { Token (Symbol ";") _ }
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
    '='     { Token (Symbol "=") _ }
    '{'     { Token (Symbol "{") _ }
    '}'     { Token (Symbol "}") _ }
    ','     { Token (Symbol ",") _ }
    "print" { Token (TokenKeyword "print") _ }
    "var"   { Token (TokenKeyword "var") _ }
    "if"    { Token (TokenKeyword "if") _ }
    "else"  { Token (TokenKeyword "else") _ }
    "while" { Token (TokenKeyword "while") _ }
    "for"   { Token (TokenKeyword "for") _ }
    "break" { Token (TokenKeyword "break") _ }
    literal { Token (Lit _) _ }
    ident   { Token (TokenIdent _) _ }


%left "or"
%left "and"
%left "==" "!="
%nonassoc '>' '<' ">=" "<="
%left '+' '-'
%left '*' '/'
%right "NEGATE" "not"



%%
Prgm :: { [Stmt] }
    : Stm Prgm     { $1 : $2 }
    | Stm          { $1 : [] }

Stm :: { Stmt }
    : "print" Exp ';'   { Print $2 (getPos $1)}
    | "var" ident ';'   { Declaration $2 Nothing (getPos $1) }
    | "var" ident '=' Exp ';' { Declaration $2 (Just $4) (getPos $1) }
    | Exp ';'           { StmtExpr $1 }
    | '{' Prgm '}'      { Block $2 (getPos $1) }
    | "if" '(' Exp ')' Stm    { IfElse $3 $5 Nothing (getPos $1) }
    | "if" '(' Exp ')' Stm "else" Stm { IfElse $3 $5 (Just $7) (getPos $1)}
    | "while" '(' Exp ')' Stm         { While $3 $5 (getPos $1)}
    | "for" '(' OptInit OptCond OptIter ')' Stm { createForLoop $3 $4 $5 $7 (getPos $1) }
    | "break" { Break (getPos $1) }

OptInit :: { [Stmt] }
        : Stm { [$1] }
        | ';' { []   }

OptCond :: { Expr }
        : Exp ';' { $1 }
        | ';'     { Litr (LBool True) (getPos $1) }

OptIter :: { [Stmt] }
        : Exp { [StmtExpr $1] }
        |     { [] }

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
    | ident          { Identifier $1 }
    | ident '=' Exp  { Assign $1 $3 (getPos $2) }

{

createForLoop :: [Stmt] -> Expr -> [Stmt] -> Stmt -> Pos -> Stmt
createForLoop initial condition increment body pos = Block outerBlock pos where
    innerBlock = Block (body:increment) pos
    whileBlock = While condition innerBlock pos
    outerBlock = initial ++ [whileBlock]

parseError :: [Token] -> Check String a
parseError [] = Error "Parse error: Unexpected end of input."
parseError ((Token tt pos):_) = Error $ "Unexpected token: " ++ (show tt) ++ stringPos pos


}