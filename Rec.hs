{-| Demo on how to evaluate recursive functions via
    eval/apply style interpretation.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rec where

import Data.String (IsString(..))

type Name = String

data Expr
  = Ref Name
  | Bool Bool
  | Int Int
  | Equal Expr Expr
  | Add Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Lam Name Expr
  | App Expr Expr
  | Rec Name Expr
  | If Expr Expr Expr
  deriving stock (Show)

instance IsString Expr where
  fromString = Ref

instance Num Expr where
  fromInteger = Int . fromInteger
  (+) = Add
  (-) = Minus
  (*) = Mul
  abs = error "abs not implemented"
  signum = error "signum not implemented"

(==>) :: Name -> Expr -> Expr
x ==> b = Lam x b

(@) :: Expr -> Expr -> Expr
f @ a = App f a

(===) :: Expr -> Expr -> Expr
l === r = Equal l r

data Value
  = Closure Env Name Expr
  | VBool Bool
  | VInt Int
  deriving stock (Show)

type Env = [(Name, Value)]

l :: (Eq a) => a -> [(a, b)] -> b
l x ((y,v):ys) = if x == y then v else l x ys

eval :: Env -> Expr -> Value
eval env (Ref x) = l x env
eval env (Lam x e) = Closure env x e
eval env (App e0 e1) = case (eval env e0, eval env e1) of
  (f@(Closure env' x v0), b) -> eval ((x, b) : env') v0
eval env (Rec x e) = let v = eval ((x, v) : env) e in v
eval env (Bool b) = VBool b
eval env (If p ifT ifF) = case eval env p of
  VBool True -> eval env ifT
  VBool False -> eval env ifF
eval env (Int i) = VInt i
eval env (Add l r) = case (eval env l, eval env r) of
  (VInt l', VInt r') -> VInt (l' + r')
eval env (Mul l r) = case (eval env l, eval env r) of
  (VInt l', VInt r') -> VInt (l' * r')
eval env (Minus l r) = case (eval env l, eval env r) of
  (VInt l', VInt r') -> VInt (l' - r')
eval env (Equal l r) = case (eval env l, eval env r) of
  (VInt l', VInt r') -> VBool (l' == r')

id_ :: Expr
id_ = "x" ==> "x"

const_ :: Expr
const_ = "x" ==> ("y" ==> "x")

ex1 :: Expr
ex1 = If (Bool False) id_ const_

ex2 :: Expr
ex2 = 2 + 3

-- sum' n = if n == 0 then 0 else n + sum' (n - 1)
sum' :: Expr
sum' = Rec "sum" $ "n" ==> If ("n" === 0) 0 ("n" + ("sum" @ ("n" - 1)))

ex3 :: Expr
ex3 = sum' @ 30
