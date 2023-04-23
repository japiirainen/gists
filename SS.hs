{-| small-step operational semantics for lambda calculus
    This is basically the simplest and dumbest implementation
    imaginable. It's sole purpose is to be as simple and readable
    as possible.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SS where

import Data.String (IsString(..))

type Name = String

data Expr
  = Var String
  | String :=> Expr -- lambda
  | Expr :@ Expr -- app

instance Show Expr where
  showsPrec _ (Var x) = showString x
  showsPrec _ (x :=> e) = showParen True $ showString "\\" . showString x . showString ". " . showsPrec 0 e
  showsPrec p (e0 :@ e1) = showParen (p > 10) $ showsPrec 10 e0 . showString " @ " . showsPrec 11 e1

instance IsString Expr where
  fromString s = Var s

isVal :: Expr -> Bool
isVal (_ :=> _) = True
isVal (Var{}) = True
isVal _ = False

-- | substitution
-- Eg. \x. x y [y/z] = \x. x z
(//) :: Expr -> (Name, Expr) -> Expr
(Var x) // (y, e) | x == y = e | otherwise = error "unbound variable"
(x :=> e0) // (y, e1) = x :=> (e0 // (y, e1))
(e0 :@ e1) // (y, e) = (e0 // (y, e)) :@ (e1 // (y, e))

-- | single step evaluation
step :: Expr -> Expr
step v@Var{} = v
step v@(_ :=> _) = v
step (v0@(x :=> e) :@ v1)
  | isVal v1 = e // (x, v1)
  | otherwise = v0 :@ (step v1)
step (e0 :@ e1) = (step e0) :@ e1

-- | evaluate until `Expr` is a value
eval :: Expr -> Expr
eval = until isVal step

-- some examples

-- | (\x. x)
id_ :: Expr
id_ = "x" :=> "x"

-- | (\x. \y. x)
const_ :: Expr
const_ = "x" :=> ("y" :=> "x")

ex1 = id_ :@ ("y" :=> "y")
ex2 = const_ :@ id_
ex3 = id_ :@ const_
ex4 = ("y" :=> "x") :@ "z"
ex5 = ("x" :=> ("x" :@ "y")) :@ "x"
