{-# LANGUAGE InstanceSigs #-}
module Interpreter(interpret, Interpreter, InterpreterState, interpret', Control(..), initInterpreter ) where

import Prelude hiding (EQ, GT, LT, lookup)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class()
import Check
import Tok(Literal(..))
import Operators
import Environment
import Data.IORef
import Data.List(stripPrefix)
import qualified Data.Text as T

data Function = Function { getClosure :: EnvrState,
                           getIdents :: [Ident],
                           getStmts :: [Stmt] }

data Value
    = BoolVal Bool
    | NumVal Double
    | StringVal T.Text
    | LambdaVal Function
    | Nil

instance Show Value where
    show :: Value -> String
    show (BoolVal v) = show v
    show (NumVal d) = case stripSuffix ".0" (show d) of
        Just trim -> trim
        Nothing -> (show d)
    show (StringVal str) = T.unpack str
    show (LambdaVal _) = "Please don't try and show functions :)"
    show Nil = "nil"

class HasType a where
    vType :: a -> String

instance HasType Value where
    vType :: Value -> String
    vType (BoolVal _) = "Boolean"
    vType (NumVal _) = "Number"
    vType (StringVal _) = "String"
    vType (LambdaVal _) = "Function"
    vType Nil = "Nil"

data InterpreterState = InterpreterState {envStack :: [EnvrState]}
type Cell = IORef Value
type EnvrState = Environment T.Text Value
data Control e
    = CtrlBreak
    | CtrlReturn Value
    | RuntimeError e
    deriving(Show)

type GenInterpreter e a = CheckT (Control e) (StateT InterpreterState IO) a
type Interpreter a = GenInterpreter String a
--type ExceptIO = ExceptT Error IO
type BinFunc a = Value -> Value -> Interpreter a
type UnaFunc a = Value -> Interpreter a

initInterpreter :: InterpreterState
initInterpreter = InterpreterState initialEnvStack

initialEnv :: EnvrState
initialEnv = emptyEnv

initialEnvStack :: [EnvrState]
initialEnvStack = [initialEnv]

-- childEnv :: EnvrState -> EnvrState
-- childEnv = Environment [] . Just

pushEnv :: EnvrState -> Interpreter ()
pushEnv env = do
    envs <- getEnvStack
    putEnvStack (env:envs)

popEnv :: Interpreter EnvrState
popEnv = do
    envs <- getEnvStack
    case envs of
        [] -> throwRuntimeErr $ "Tried to pop environment while stack is empty."
        e:es -> do
            putEnvStack es
            return e

beginEnv :: Interpreter ()
beginEnv = pushEnv emptyEnv

endEnv :: Interpreter ()
endEnv = do
    _ <- popEnv
    return ()

getEnvStack :: Interpreter [EnvrState]
getEnvStack = do
    s <- get
    return (envStack s)

putEnvStack :: [EnvrState] -> Interpreter ()
putEnvStack env = do
    s <- get
    put (s {envStack = env})

runInterpreter :: Interpreter () -> InterpreterState -> IO (Check (Control String) (), InterpreterState)
runInterpreter = runStateT . runCheckT

interpret :: [Stmt] -> IO ()
interpret intr = do
    (res, _) <- (runInterpreter (executeProgram intr)) (InterpreterState initialEnvStack)
    case res of
        Error c -> case c of
            RuntimeError e -> putStrLn e
            o -> putStrLn $ "Probably should have caught " ++ (show o ) ++ " earlier, huh? Look what you fucking did. You buffoon."
        Checked _ -> return ()

interpret' :: [Stmt] -> InterpreterState -> IO (Check (Control String) (), InterpreterState)
interpret' pgrm = runInterpreter (executeProgram pgrm)

executeProgram :: [Stmt] -> Interpreter ()
executeProgram stmts = do
    _ <- mapM execute stmts
    return ()

execute :: Stmt -> Interpreter ()
execute (Print expr _) = do
    value <- evaluate expr
    liftIO $ putStrLn (show value)
execute (StmtExpr expr) = do
    _ <- evaluate expr
    return ()
execute (Declaration ident initial _) = do
    mVal <- evaluate initial
    newRef <- liftIO $ newIORef mVal
    top <- popEnv
    pushEnv (insertRef top (getName ident, newRef)) 
execute (Block stmts _) = do
    beginEnv
    executeProgram stmts
    endEnv
execute (IfElse condition _if m_else _) = do
    ex <- truth condition
    if ex
        then execute _if
        else case m_else of
            Nothing -> return()
            (Just _else) -> execute _else
execute (While condition stmt _) = helper where
    helper = do
        ex <- truth condition
        if ex
            then do 
                brk <- catchBreak (execute stmt)
                if brk then return() else helper
            else return ()
execute (Break _) = throwError CtrlBreak
execute (Return expr _) = do
    val <- evaluate expr
    throwError (CtrlReturn val)

truthiness :: Value -> Bool
truthiness (NumVal 0) = False
truthiness (StringVal T.Empty) = False
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
    l `op` r `catchRuntimeErr` c where
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
        c = \e -> throwRuntimeErr (e ++ show pos)
evaluate (Unary operator op1 pos) = do
    r <- evaluate op1
    op r `catchRuntimeErr` c where
        op = case operator of
            Neg -> neg
            Not -> (%!)
        c = (\e -> throwRuntimeErr $ e ++ show pos)
evaluate (Group expr _) = evaluate expr
evaluate (Assign ident expr _) = do
    v <- evaluate expr
    ref <- envLookup ident
    liftIO $ writeIORef ref v
    return v
evaluate (Identifier ident) = do
    ref <- envLookup ident
    liftIO $ readIORef ref
evaluate (Lambda nonlocals params stmts _) = do
    beginEnv
    refs <- mapM envLookup nonlocals
    endEnv
    let names = map getName nonlocals
        closure = fromAscList (zip names refs)
    (return . LambdaVal) (Function closure params stmts)
evaluate (Call expr argExprs pos) = do
    callee <- evaluate expr
    case callee of
        LambdaVal (Function closure params body) -> do
            args <- mapM evaluate argExprs
            newRefs <- liftIO $ mapM newIORef args
            locals <- zipParams params newRefs `catchRuntimeErr` (\e -> throwRuntimeErr $ e ++ " " ++ show pos)
            curStack <- getEnvStack
            putEnvStack (locals:closure:[])
            returnVal <- catchReturn (executeProgram body)
            putEnvStack curStack
            return returnVal
        val -> throwRuntimeErr $ "Tried to call noncallable type: " ++ vType val ++ " " ++ show pos

zipParams :: [Ident] -> [Cell] -> Interpreter EnvrState
zipParams idents cells = do
    zipped <- helper (map getName idents) cells
    return (fromList zipped) where
        helper [] [] = return []
        helper [] (_:_) = throwRuntimeErr $ "Too many arguments given for this function."
        helper (_:_) [] = throwRuntimeErr $ "Not enough arguments given for this call."
        helper (i:is') (c:cs) = do
            rest <- helper is' cs
            return $ (i,c):rest

envLookup :: Ident -> Interpreter Cell
envLookup (Ident name depth pos) = do
    envs <- getEnvStack
    helper envs depth where
        helper :: [EnvrState] -> Int -> Interpreter Cell
        helper _ (-1) = throwRuntimeErr $ "Failed to resolve \"" ++ T.unpack name ++ "\"!" ++ show pos
        helper [] _ = throwRuntimeErr $ "Ran out of environments to look through! Ident: \"" ++ T.unpack name ++ "\""
        helper (e:_) 0 = case findRef e name of
            Just ref -> return ref
            Nothing -> throwRuntimeErr $ "Could not find \"" ++ T.unpack name ++ "\" declared in said scope, tried to go " ++ (show depth) ++ " shells up." ++ show pos
        helper (_:es) i = helper es (i-1)

-- printEnvStack :: [EnvrState] -> String
-- printEnvStack [] = ""
-- printEnvStack (e:es) = "(" ++ printEnv e ++ ")" ++ printEnvStack es

-- printEnv :: EnvrState -> String
-- printEnv (Environment list) = helper list where
--     helper [] = "[]"
--     helper ((val, _):rest) = (show val) ++ helper rest


catchRuntimeErr :: GenInterpreter e a -> (e -> GenInterpreter e a) -> GenInterpreter e a
(CheckT ma) `catchRuntimeErr` handler = CheckT $ do
    a <- ma
    case a of
        Checked x -> return (Checked x)
        Error y -> case y of
            RuntimeError e -> runCheckT (handler e)
            z -> return (Error z)


catchBreak :: GenInterpreter e () -> GenInterpreter e Bool
catchBreak (CheckT ma) = CheckT $ do
    a <- ma
    case a of
        Checked _ -> return (Checked False)
        Error CtrlBreak -> do
            return (Checked True)
        Error (RuntimeError e) -> return $ Error (RuntimeError e)
        Error other -> return (Error other)

catchReturn :: GenInterpreter e () -> GenInterpreter e Value
catchReturn (CheckT ma) = CheckT $ do
    a <- ma
    case a of
        Checked _ -> return (Checked Nil)
        Error (CtrlReturn val) -> return (Checked val)
        Error other -> return (Error other)
        

(%==) :: BinFunc Value
(NumVal x) %== (NumVal y) = return $ BoolVal (x == y)
(StringVal x) %== (StringVal y) = return $ BoolVal (x == y)
(BoolVal x) %== (BoolVal y) = return $ BoolVal (x == y)
x %== y = throwRuntimeErr $ binErrStr x y "=="

(%/=) :: BinFunc Value
x %/= y = do
    z <- x %== y
    (%!) z `catchRuntimeErr` (\_ -> throwRuntimeErr $ binErrStr x y "!=")

(%<) :: BinFunc Value
(NumVal x) %< (NumVal y) = return $ BoolVal (x < y)
x %< y = throwRuntimeErr (binErrStr x y "<")

(%>) :: BinFunc Value
(NumVal x) %> (NumVal y) = return $ BoolVal (x > y)
x %> y = throwRuntimeErr (binErrStr x y ">")

(%<=) :: BinFunc Value
x %<= y = do
    z <- x %> y
    (%!) z
    `catchError` (\_ -> throwRuntimeErr $ binErrStr x y "<=")

(%>=) :: BinFunc Value
x %>= y = do
    z <- x %< y
    (%!) z
    `catchError` (\_ -> throwRuntimeErr $ binErrStr x y ">=")

showT :: (Show a) => a -> T.Text
showT = T.pack . show

(%+) :: BinFunc Value
(NumVal x) %+ (NumVal y) = return $ NumVal (x + y)
(NumVal x) %+ (StringVal y) = return $ StringVal $ (showT x) `T.append` y
(StringVal x) %+ (NumVal y) = return $ StringVal $ x `T.append` (showT y)
(StringVal x) %+ (StringVal y) = return $ StringVal (x `T.append` y)
x %+ y = throwRuntimeErr $ binErrStr x y "+"

(%-) :: BinFunc Value
(NumVal x) %- (NumVal y) = return $ NumVal (x - y)
x %- y = throwRuntimeErr $ binErrStr x y "-"

(%*) :: BinFunc Value
(NumVal x) %* (NumVal y) = return $ NumVal (x * y)
x %* y = throwRuntimeErr $ binErrStr x y "*"

(%/) :: BinFunc Value
(NumVal x) %/ (NumVal y) = return $ NumVal (x / y)
x %/ y = throwRuntimeErr $ binErrStr x y "/"

(%!) :: UnaFunc Value
(%!) x = let b = truthiness x in return $ BoolVal (not b)

neg :: UnaFunc Value
neg (NumVal x) = return $ NumVal (-x)
neg y = throwRuntimeErr $ unaErrStr y "-"

throwRuntimeErr :: String -> Interpreter a
throwRuntimeErr = throwError . RuntimeError

binErrStr :: Value -> Value -> String -> String
binErrStr x y op = 
    "Operands " ++ (show x) ++ " and " ++ (show y) ++ " not defined for operation " ++ op
unaErrStr :: Value -> String -> String
unaErrStr x op = 
    "Operand " ++ (show x) ++ " not defined for operation " ++ op

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf xs =
  reverse <$> stripPrefix (reverse suf) (reverse xs)