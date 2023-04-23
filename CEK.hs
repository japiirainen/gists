{-| Experimenting with abstract machines

    The CEK machine
    C - control
    E - environment
    K - continuation

    A CEK machine is an abstract machine that implements left-to-right call by value.
    It is videly used for implementing interpreters for functional programming languages.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module CEK where

import Data.String (IsString(..))

type Name = String

data Lambda = Name :=> Expr
  deriving stock (Show)

data Expr where
  Ref :: Name -> Expr
  Lam :: Lambda -> Expr
  (:@) :: Expr -> Expr -> Expr

deriving instance Show Expr

instance IsString Expr where
  fromString = Ref

data Value = Closure Lambda Env
  deriving stock (Show)

type Env = Name -> Value

instance Show (a -> b) where
  show = const "<fun>"

data Kont
  = Empty
  | Arg Expr Env Kont
  | Fun Lambda Env Kont
  deriving stock (Show)

data State = State Expr Env Kont
  deriving stock (Show)

type Program = Expr

inj :: Program -> State
inj p = State p (const undefined) Empty

isFinal :: State -> Bool
isFinal (State Lam{} _ Empty) = True
isFinal _ = False

step :: State -> State
-- evaluating a reference => lookup in environment
step (State (Ref x) l k) = State (Lam lam) l k where (Closure lam env) = l x
-- evaluating a function application => first evaluate argument
step (State (f :@ a) l k) = State f l (Arg a l k)
-- evaluating function => evaluate the argument term
step (State (Lam lam) l (Arg a e k)) = State a e (Fun lam l k)
-- evaluating argument => perform the application
step (State (Lam lam) l (Fun (x :=> b) _ k)) = State b (subst l (x, Closure lam l)) k
step state = error ("evaluation entered bad state " <> (show state))

subst :: Env -> (Name, Value) -> Env
subst f (x, v) = \y -> if x == y then v else f y

terminal :: (State -> State) -> (State -> Bool) -> State -> State
terminal s done = until done s

eval :: Program -> State
eval program = terminal step isFinal (inj program)

id_ :: Expr
id_ = Lam $ "x" :=> "x"

const_ :: Expr
const_ = Lam $ "x" :=> (Lam $ "y" :=> "x")
