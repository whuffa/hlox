module Resolver(resolve, ResolverState, resolve', initResolver) where

import Prelude hiding (lookup)
import Operators ( Stmt(..), Expr(..), Ident(..) )
import Check
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict
import qualified Data.Set as S
import qualified Data.Text as T

type ScopeSet = Map T.Text Bool

data FuncType
    = NotFunc
    | Function

data FrameType
    = Scope
    | FuncBody
    deriving (Show, Eq)

type Closure = S.Set Ident
type Frame = (FrameType, ScopeSet)

data ResolverState = ResolverState {stateFunc :: FuncType,
                                    frameStack :: [Frame],
                                    closureStack :: [Closure],
                                    loopDepth :: Int }
type Resolver a = CheckT String (StateT ResolverState IO) a

initialEnv :: ScopeSet
initialEnv = empty

initResolver :: ResolverState
initResolver = ResolverState (NotFunc) [(Scope, initialEnv)] [] 0

runResolver :: Resolver [Stmt] -> IO (Check String [Stmt])
runResolver rstmt = do
    (stmts, _) <- runResolverState rstmt (initResolver)
    return stmts

runResolverState :: Resolver [Stmt] -> ResolverState -> IO (Check String [Stmt], ResolverState)
runResolverState = runStateT . runCheckT

-- class Resolvable a where --maybe more readable if we do this?
--     resolved :: (a -> Resolver a)

resolve' :: [Stmt] -> ResolverState -> IO (Check String [Stmt], ResolverState)
resolve' stmts = runResolverState (resolveProgram stmts)


resolve :: [Stmt] -> IO (Check String [Stmt])
resolve = runResolver . resolveProgram

getter :: (ResolverState -> a) -> Resolver a
getter f = do
    _state <- get
    return (f _state)

getClosures :: Resolver [Closure]
getClosures = getter closureStack

putClosures :: [Closure] -> Resolver ()
putClosures closures = do
    _st <- get
    put (_st {closureStack = closures})

popClosure :: Resolver Closure
popClosure = do
    closures <- getClosures
    case closures of
        [] -> throwError $ "Ran out of closures to pop."
        c:cs -> do
            putClosures cs
            return c

pushClosure :: Closure -> Resolver ()
pushClosure closure = do
    closures <- getClosures
    putClosures (closure:closures)


beginClosure :: Resolver ()
beginClosure = do
    pushFrame (FuncBody, initialEnv)
    pushClosure S.empty


endClosure :: Resolver [Ident]
endClosure = do
    closure <- popClosure
    (ft, _) <- popFrame
    case ft of
        Scope -> throwError $ "Something went wrong - tried to pop a function body, got normal scope."
        FuncBody -> return (S.elems closure)


incrLoop :: Resolver()
incrLoop = do
    depth <- getLoopDepth
    putLoopDepth (depth + 1)

decrLoop :: Resolver()
decrLoop = do
    depth <- getLoopDepth
    if (depth > 0)
        then putLoopDepth (depth - 1)
        else throwError $ "Something went wrong resolving - decremented loop depth to negative."


getLoopDepth :: Resolver Int
getLoopDepth = getter loopDepth

putLoopDepth :: Int -> Resolver ()
putLoopDepth count = do
    _state <- get
    put (_state {loopDepth = count})

getStateFunc :: Resolver FuncType
getStateFunc = getter stateFunc

putStateFunc :: FuncType -> Resolver ()
putStateFunc fType = do
    _state <- get
    put _state {stateFunc = fType}

getFrames :: Resolver [Frame]
getFrames = getter frameStack

putFrames :: [Frame] -> Resolver()
putFrames scopes = do
    _state <- get
    put _state {frameStack = scopes}

pushFrame :: Frame -> Resolver()
pushFrame _map = do
    scopes <- getFrames
    putFrames (_map:scopes)

popFrame :: Resolver Frame
popFrame = do
    frames <- getFrames
    case frames of
        [] -> throwError "Something went wrong while resolving - ran out of scopes to pop."
        s:ss -> do
            putFrames ss
            return s

beginScope :: Resolver ()
beginScope = pushFrame (Scope, empty)

endScope :: Resolver ()
endScope = do 
    _ <- popFrame
    return ()

resolveProgram :: [Stmt] -> Resolver [Stmt]
resolveProgram = mapM resolveStmt 

resolveStmt :: Stmt -> Resolver Stmt
resolveStmt (Print expr pos) = do
    re <- resolveExpr expr
    return (Print re pos)
resolveStmt (StmtExpr expr) = do
    resolved <- resolveExpr expr
    return (StmtExpr resolved)
resolveStmt (Declaration ident initializer pos) = do
    declare ident
    ri <- resolveExpr initializer
    define ident
    return (Declaration ident ri pos)
resolveStmt (Block stmts pos) = do
    beginScope
    rs <- resolveProgram stmts
    endScope
    return (Block rs pos)
resolveStmt (IfElse condition _if m_else pos) = do
    rc <- resolveExpr condition
    rif <- resolveStmt _if
    mre <- case m_else of
        Just _else -> do
            me <- resolveStmt _else
            return (Just me)
        Nothing -> return (m_else)
    return (IfElse rc rif mre pos)
resolveStmt (While condition stmt pos) = do
    incrLoop
    rc <- resolveExpr condition
    rs <- resolveStmt stmt
    decrLoop
    return (While rc rs pos)
resolveStmt n@(Break pos) = do
    depth <- getLoopDepth
    if (depth > 0) then return n else throwError $ "Break statements must be used within loops. " ++ show pos
resolveStmt (Return expr pos) = do
    isFunc <- getStateFunc
    case isFunc of
        NotFunc -> throwError $ "Return statements must be used within functions. " ++ show pos
        _ -> do
            re <- resolveExpr expr
            return (Return re pos)


declare :: Ident -> Resolver()
declare (Ident name _ _) = do
    (ft, scope) <- popFrame
    pushFrame (ft, (insert name False scope))


define :: Ident -> Resolver()
define (Ident name _ _) = do
    (ft, scope) <- popFrame
    pushFrame (ft, (insert name True scope))

resolveExpr :: Expr -> Resolver Expr
resolveExpr n@(Litr _ _) = return n

resolveExpr (Binary op l r pos) = do
    rl <- resolveExpr l
    rr <- resolveExpr r
    return (Binary op rl rr pos)

resolveExpr (Unary op o pos) = do
    ro <- resolveExpr o
    return (Unary op ro pos)

resolveExpr (Group expr pos) = do
    re <- resolveExpr expr
    return (Group re pos)

resolveExpr (Identifier ident) = do
    rIdent <- resolveIdent ident
    return (Identifier rIdent)

resolveExpr (Assign ident expr pos) = do
    re <- resolveExpr expr
    rIdent <- resolveIdent ident
    return (Assign rIdent re pos)

resolveExpr (Lambda _ idents stmts pos) = do
    curFunc <- getStateFunc
    putStateFunc Function
    beginClosure
    _ <- mapM define idents
    rstmts <- mapM resolveStmt stmts
    closure <- endClosure
    liftIO $ putStrLn $ show closure
    putStateFunc curFunc
    return (Lambda closure idents rstmts pos)
    
resolveExpr (Call expr args pos) = do
    re <- resolveExpr expr
    ra <- mapM resolveExpr args
    return (Call re ra pos)

resolveIdent :: Ident -> Resolver Ident
resolveIdent ident = do
    frames <- getFrames
    closures <- getClosures
    (ident', closures') <- lookupIdent ident frames closures
    putClosures closures'
    return ident'

lookupIdent :: Ident -> [Frame] -> [Closure] -> Resolver (Ident, [Closure])
lookupIdent (Ident name _ pos) = helper 0 where
    helper :: Int -> [Frame] -> [Closure] -> Resolver (Ident, [Closure])
    helper _ [] _ = throwError $ "Could not resolve variable \"" ++ T.unpack name ++ "\"."
    helper i ((ft, scope):fs) closures = do
        case lookup name scope of
            Just False -> throwError $ "Cannot initialize a variable with itself!"
            Just True -> do
                return ((Ident name i pos),closures)
            Nothing -> case ft of
                Scope -> helper (i+1) fs closures
                FuncBody -> case closures of
                    [] -> throwError $ "Expected another closure to pop!"
                    c:cs -> do
                        cs' <- if any (\x -> getName x == name) c
                            then return (c:cs)
                            else do
                                (ident', cs') <- helper 1 fs cs
                                return ((S.insert ident' c):cs')
                        return ((Ident name (i+1) pos), cs')

