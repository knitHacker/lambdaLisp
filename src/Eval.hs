module Eval
    ( LValue(..)
    , Env(..)
    , eval
    ) where

import Parser

import qualified Data.Map.Strict as M

import Debug.Trace

data Env = Env
    { symbolTable :: (M.Map String SExpression)
    } deriving (Show, Eq)

data LValue =
    VNum Integer
    | VString String
    | EList [SExpression]
    | VList [LValue]
    | VError String
    | VNil
    deriving (Show, Eq)

updateSymbol :: Env -> String -> SExpression -> Env
updateSymbol env name value = env { symbolTable = symbolTable' }
    where
        st = symbolTable env
        symbolTable' = M.insert name value st


lookupSymbol :: Env -> String -> Maybe SExpression
lookupSymbol env name = M.lookup name st
    where
        st = symbolTable env

opLVal :: String -> (Integer -> Integer -> Integer) -> [LValue] -> LValue
opLVal n o ls = case ls of
    [] -> VError ("Wrong number of arguments for " ++ n)
    (h:tl) -> opLVal' n o tl h

opLVal' :: String -> (Integer -> Integer -> Integer) -> [LValue] -> LValue -> LValue
opLVal' _ _ [] n = n
opLVal' name op (VNum n2:tl) (VNum n1) = opLVal' name op tl (VNum (op n1 n2))
opLVal' name _ (VError e:_) _ = VError ("Ran into error when applying " ++ name ++ ": " ++ e)
opLVal' name _ _ _ = VError ("Wrong type for operator " ++ name)

plus :: [LValue] -> LValue
plus = opLVal "+" (+)

multiply :: [LValue] -> LValue
multiply = opLVal "*" (*)

minus :: [LValue] -> LValue
minus (VNum n:[]) = VNum (-n)
minus lvals = opLVal "-" (-) lvals

divide :: [LValue] -> LValue
divide  [] = VError "Wrong number of arguments for /"
divide (VNum 0:_) = VError "Divide by 0"
divide (h:tl) = divide' tl h

divide' :: [LValue] -> LValue -> LValue
divide' [] v@(VNum _) = v
divide' (VNum 0:_) _ = VError "Divide by 0"
divide' (VNum n2:tl) (VNum n1) = divide' tl (VNum (div n1 n2))
divide' _ _ = VError "Wrong type for operator /"

evalIf :: Env -> [SExpression] -> (Env, LValue)
evalIf env (p:t:tl) = case tl of
    [] -> if isNil then (env', VNil) else tRes
    [f] -> if isNil then eval env' f else tRes
    _ -> (env', VError "Wrong number of arguments for IF")
    where
        tRes = eval env' t
        (env', vP) = eval env p
        isNil = vP == VNil
evalIf env _ = (env, VError "Wrong number of arguments for IF")

initLets :: Env -> [SExpression] -> Env
initLets env [] = env
initLets env (SList [SAtom (ASymbol (Symbol s)), e]:tl) = initLets env' tl
    where
        env' = updateSymbol env s e
initLets _ _ = error "var definitions wrong"
        

evalLet :: Env -> [SExpression] -> (Env, LValue)
evalLet env (SList vars:decls) = (env, res)
    where
        (_, res) = foldl eval' (env', VNil) decls
        env' = initLets env vars
        eval' (pe, _) e = eval pe e

evalSetQ :: Env -> [SExpression] -> (Env, LValue)
evalSetQ env exs = evalSetQ' VNil env exs

evalSetQ' :: LValue -> Env -> [SExpression] -> (Env, LValue)
evalSetQ' lval env [] = (env, lval)
evalSetQ' _ env ((SAtom (ASymbol (Symbol var))):val:tl) = evalSetQ' lv env' tl
    where 
        (_, lv) = eval env val
        env' = updateSymbol env var val 
evalSetQ' _ env _ = (env, VError "Bad setq expression")

evalOperator :: Operator -> [LValue] -> LValue
evalOperator Plus = plus
evalOperator Minus = minus
evalOperator Multiply = multiply
evalOperator Divide = divide
evalOperator o = \_ -> VError ("Unexpected operator: " ++ show o)


evalBuiltIn :: Env -> Operator -> [SExpression] -> (Env, LValue)
evalBuiltIn env If exs = evalIf env exs
evalBuiltIn env Quote exs = (env, EList exs)
evalBuiltIn env Let exs = evalLet env exs
evalBuiltIn env SetQ exs = evalSetQ env exs
evalBuiltIn env op exs = (env', evalOperator op lvals)
    where
        foldEval (e, lvs) ex = 
            let (e', lv) = eval e ex
            in (e', lvs ++ [lv])
        (env', lvals) = foldl foldEval (env, []) exs


evalVariable :: Env -> String -> (Env, LValue)
evalVariable env name = case lookupSymbol env name of
    Nothing -> (env, VError ("Variable " ++ name ++ " not defined"))
    (Just ex) -> eval env ex

evalFunction :: Env -> String -> [SExpression] -> (Env, LValue)
evalFunction env name [] = evalVariable env name
evalFunction env name exs = undefined

eval :: Env -> SExpression -> (Env, LValue)
eval env (SAtom n) = evalAtom env n
eval env (SList el) = evalList env el

evalList :: Env -> [SExpression] -> (Env, LValue)
evalList env [] = (env, VNil)
evalList env ((SAtom (ASymbol (SOperator op))):exs) = evalBuiltIn env op exs
evalList env ((SAtom (ASymbol (Symbol sym))):exs) = evalFunction env sym exs 
evalList env (e:exs) = case exs of
    [] -> (env', lval)
    _ -> error ("Can't appy " ++ show lval ++ "to " ++ show exs)
    where
        (env', lval) = eval env e


evalAtom :: Env -> Atom -> (Env, LValue)
evalAtom e (ANum n) = (e, VNum n)
evalAtom e ANil = (e, VNil)
evalAtom e (AString s) = (e, VString s)
evalAtom e (ASymbol s) = evalSymbol e s

evalSymbol :: Env -> Symbol -> (Env, LValue)
evalSymbol e (Symbol n) = evalVariable e n
evalSymbol e (SOperator o) = evalBuiltIn e o []
