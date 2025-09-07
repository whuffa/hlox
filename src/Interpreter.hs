module Interpreter(run, interpret, Interpreter) where

import Prelude hiding (EQ, GT, LT, lookup)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class()
import Check
import Tok
import Operators
import Environment hiding(declare, define)
import Value(Value(..), stringifyVal)
import Variable
import ListMap

data InterpreterState = InterpreterState {stateEnv :: EnvrState}
type EnvrState = Environment String Value

type Interpreter a = StateT InterpreterState (CheckT String IO) a
--type ExceptIO = ExceptT Error IO
type BinFunc a = Value -> Value -> Interpreter a
type UnaFunc a = Value -> Interpreter a



initialEnv :: Environment String Value
initialEnv = Environment [] Nothing

getEnv :: Interpreter (Environment String Value)
getEnv = do
    s <- get
    return (stateEnv s)

putEnv :: Environment String Value -> Interpreter ()
putEnv env = do
    s <- get
    put (s {stateEnv = env})

run :: Interpreter () -> IO ()
run interpreter = do
    result <- runCheckT $ fmap fst $ runStateT interpreter (InterpreterState initialEnv)
    case result of
        Error msg -> putStrLn msg
        Checked _ -> return()


interpret :: [Stmt] -> Interpreter ()
interpret = executeProgram

executeProgram :: [Stmt] -> Interpreter ()
executeProgram [] = return ()
executeProgram (s:ss) = do
    execute s
    executeProgram ss

execute :: Stmt -> Interpreter ()
execute (Print expr pos) = do
    value <- evaluate expr
    liftIO $ putStrLn (stringifyVal value)
execute (StmtExpr expr) = do
    _ <- evaluate expr
    return ()
execute (Declaration (Token (TokenIdent ident) tpos) initial pos) = do
    env <- getEnv
    newEnv <- declare env ident
    newerEnv <- helper newEnv initial
    putEnv newerEnv
    where
        helper :: EnvrState -> Maybe Expr -> Interpreter EnvrState
        helper env Nothing = return env
        helper env (Just expr) = do
            value <- evaluate expr
            define env ident value
execute (Declaration _ _ _)  = throwError "Something went wrong parsing!"
execute (Block stmts _) = do
    env <- getEnv
    putEnv initialEnv
    executeProgram stmts
    putEnv env

assign :: String -> Value -> Interpreter ()
assign ident value = do
    env <- getEnv
    newEnv <- define env ident value
    putEnv newEnv
    
declare :: EnvrState -> String -> Interpreter (EnvrState)
declare (Environment lmap p) k = case find lmap k of
    Undeclared -> return (Environment nmap p) where
        nmap = insert lmap (k, Nothing)
    _ -> throwError "Variable already declared in this scope."


define :: EnvrState -> String -> Value -> Interpreter (EnvrState)
define env key val = let
    assoc = (key, Just val)
    helper :: Maybe EnvrState -> Interpreter (EnvrState)
    helper Nothing = throwError "Cannot define undeclared variable."
    helper (Just (Environment lmap p)) = case find lmap key of
        Undeclared -> helper p
        _ -> return (Environment nmap p) where
            nmap = insert lmap assoc
    in helper (Just env)

truthiness :: Value -> Bool
truthiness (NumVal 0) = False
truthiness (StringVal "") = False
truthiness (BoolVal bool) = bool
truthiness Nil = False
truthiness _ = True

truth :: Expr -> Interpreter Bool
truth expr = do
    value <- evaluate expr
    return $ truthiness value

evaluate :: Expr -> Interpreter Value
evaluate (Litr lit _) = case lit of
    (Number num) -> return $ NumVal num
    (Str str) -> return $ StringVal str
    (LBool b) -> return $ BoolVal b
    LNil -> return Nil
evaluate (Binary Or op1 op2 _) = do
    b1 <- truth op1
    if b1
        then return (BoolVal True)
        else do
            b2 <- truth op2
            return (BoolVal b2)
evaluate (Binary And op1 op2 _) = do
    b1 <- truth op1
    if b1
        then do
            b2 <- truth op2
            return (BoolVal b2)
        else return (BoolVal False)

evaluate (Binary operator op1 op2 pos) = do
    l <- evaluate op1
    r <- evaluate op2
    l `op` r `catchError` c where
        op = case operator of
            Add -> (%+)
            Sub -> (%-)
            Mul -> (%*)
            Div -> (%/)
            LT -> (%<)
            GT -> (%>)
            LE -> (%<=)
            GE -> (%>=)
            EQ -> (%==)
            NE -> (%/=)
        c = \e -> throwError (e ++ stringPos pos)
evaluate (Unary operator op1 pos) = do
    r <- evaluate op1
    op r `catchError` c where
        op = case operator of
            Neg -> neg
            Not -> (%!)
        c = (\e -> throwError $ e ++ stringPos pos)
evaluate (Group expr pos) = evaluate expr
evaluate (Assign (Token (TokenIdent ident) _) expr pos) = do
    v <- evaluate expr
    assign ident v
    return v
evaluate (Assign (Token _ _) _ pos) = throwError $ "Something went wrong while parsing " ++ stringPos pos
evaluate (Identifier (Token (TokenIdent ident) tpos)) = do
    env <- getEnv
    case lookup env ident of
        Defined v -> return v
        Declared -> return Nil
        Undeclared -> throwError $ "Undeclared variable \"" ++ ident ++ "\" at " ++ stringPos tpos
evaluate (Identifier (Token _ tpos)) = throwError $ "Wrong token in Identifier parse node!" ++ (stringPos tpos)




(%==) :: BinFunc Value
(NumVal x) %== (NumVal y) = return $ BoolVal (x == y)
(StringVal x) %== (StringVal y) = return $ BoolVal (x == y)
(BoolVal x) %== (BoolVal y) = return $ BoolVal (x == y)
x %== y = lift $ throwError $ binErrStr x y "=="

-- (%/=) :: BinFunc Value
-- (NumVal x) %/= (NumVal y) = return $ BoolVal (x /= y)
-- (StringVal x) %/= (StringVal y) = return $ BoolVal (x /= y)
-- (BoolVal x) %/= (BoolVal y) = return $ BoolVal (x /= y)
-- x %/= y = lift $ Error $ 
--     "Operands " ++ (show x) ++ " and " ++ (show y) ++ " not defined for '!='"
(%/=) :: BinFunc Value
x %/= y = do
    z <- x %== y
    (%!) z `catchError` (\_ -> throwError $ binErrStr x y "!=")

(%<) :: BinFunc Value
(NumVal x) %< (NumVal y) = return $ BoolVal (x < y)
x %< y = throwError (binErrStr x y "<")

(%>) :: BinFunc Value
(NumVal x) %> (NumVal y) = return $ BoolVal (x > y)
x %> y = throwError (binErrStr x y ">")

(%<=) :: BinFunc Value
x %<= y = do
    z <- x %> y
    (%!) z
    `catchError` (\_ -> throwError $ binErrStr x y "<=")

(%>=) :: BinFunc Value
x %>= y = do
    z <- x %< y
    (%!) z
    `catchError` (\_ -> throwError $ binErrStr x y ">=")

(%+) :: BinFunc Value
(NumVal x) %+ (NumVal y) = return $ NumVal (x + y)
(NumVal x) %+ (StringVal y) = return $ StringVal $ show x ++ y
(StringVal x) %+ (NumVal y) = return $ StringVal $ x ++ (show y)
(StringVal x) %+ (StringVal y) = return $ StringVal (x ++ y)
x %+ y = throwError $ binErrStr x y "+"

(%-) :: BinFunc Value
(NumVal x) %- (NumVal y) = return $ NumVal (x - y)
x %- y = throwError $ binErrStr x y "-"

(%*) :: BinFunc Value
(NumVal x) %* (NumVal y) = return $ NumVal (x * y)
x %* y = throwError $ binErrStr x y "*"

(%/) :: BinFunc Value
(NumVal x) %/ (NumVal y) = return $ NumVal (x / y)
x %/ y = throwError $ binErrStr x y "/"

(%!) :: UnaFunc Value
(%!) x = let b = truthiness x in return $ BoolVal (not b)

neg :: UnaFunc Value
neg (NumVal x) = return $ NumVal (-x)
neg y = throwError $ unaErrStr y "-"

binErrStr :: Value -> Value -> String -> String
binErrStr x y op = 
    "Operands " ++ (show x) ++ " and " ++ (show y) ++ " not defined for operation " ++ op
unaErrStr :: Value -> String -> String
unaErrStr x op = 
    "Operand " ++ (show x) ++ " not defined for operation " ++ op

