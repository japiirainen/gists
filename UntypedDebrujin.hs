#!/usr/bin/env cabal repl
{- cabal:
build-depends: base, megaparsec
-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module UntypedDebrujin where

import Data.Void
import Data.Char
import Text.Megaparsec

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as P

-- | De Bruijn index
newtype Ix = Ix Int
  deriving (Eq, Show, Num) via Int

-- | De Bruijn level
newtype Lvl = Lvl Int
  deriving (Eq, Show, Num) via Int

data Expr
  = Var Ix
  | Lam Expr
  | App Expr Expr
  | Let Expr Expr

data Closure = Closure Env Expr

data Env
  = Nil
  | Cons Env Value

envLength :: Env -> Int
envLength Nil = 0
envLength (Cons env _) = 1 + envLength env

data Value
  = VVar Lvl
  | VApp Value ~Value
  | VLam Closure

lookupVar :: Env -> Ix -> Value
lookupVar (Cons env v) 0 = v
lookupVar (Cons env _) i = lookupVar env (i - 1)
lookupVar _ _ = error "lookupVar: index out of range"

closApply :: Closure -> Value -> Value
closApply (Closure env e) ~v = eval (Cons env v) e

eval :: Env -> Expr -> Value
eval env (Var i) = lookupVar env i
eval env (Lam e) = VLam (Closure env e)
eval env (App f a) = case (eval env f, eval env a) of
  (VLam f', a') -> closApply f' a'
  (f', a') -> VApp f' a'
eval env (Let e0 e1) = eval (Cons env (eval env e0)) e1

lvlToIx :: Lvl -> Lvl -> Ix
lvlToIx (Lvl lvl) (Lvl i) = Ix (lvl - i - 1)

quote :: Lvl -> Value -> Expr
quote lvl (VVar i) = Var (lvlToIx lvl i)
quote lvl (VApp f a) = App (quote lvl f) (quote lvl a)
quote lvl (VLam f) = Lam (quote (lvl + 1) (closApply f (VVar lvl)))

normalize :: Env -> Expr -> Expr
normalize env e = quote (Lvl (envLength env)) (eval env e)

parseExpr :: String -> Expr
parseExpr s = case parse pExpr "" s of
  Left err -> error (errorBundlePretty err)
  Right e -> e

run :: String -> Expr
run s = normalize Nil (parseExpr s)

example :: String
example = unlines
  [ "let fn fn 1 (1 (1 (1 (1 0))));" -- 5
  , "let fn fn fn fn 3 1 (2 1 0);"   -- add
  , "let 0 1 1;"                      -- 5 + 5
  , "0"
  ]

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space P.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol = L.symbol ws

char :: Char -> Parser Char
char c = lexeme (P.char c)

parens :: Parser a -> Parser a
parens p = (char '(') *> p <* (char ')')

pKeyword :: String -> Parser ()
pKeyword s = P.string s >> (takeWhile1P Nothing isDigit *> empty) <|> ws

pIx :: Parser Ix
pIx = Ix <$> lexeme L.decimal

pAtom :: Parser Expr
pAtom = Var <$> pIx <|> parens pExpr

pSpine :: Parser Expr
pSpine = foldl1 App <$> some pAtom

pLam :: Parser Expr
pLam = do
  pKeyword "fn"
  Lam <$> pExpr

pLet :: Parser Expr
pLet = do
  pKeyword "let"
  e0 <- pExpr
  pKeyword ";"
  Let e0 <$> pExpr

pSrc :: Parser Expr
pSrc = ws *> pExpr <* eof

pExpr :: Parser Expr
pExpr = pLet <|> pLam <|> pSpine

instance Show Expr where
  showsPrec prec = go (prec /= 0) where
    goArg = go True
    goLam (Lam t) = ("fn "++) . goLam t
    goLam t = go False t
    go p e = case e of
      Var x -> (show x++)
      App (App t u) u' -> showParen p (go False t . (' ':) . goArg u . (' ':) . goArg u')
      App t u -> showParen p (go True t . (' ':) . goArg u)
      Lam t -> showParen p (("fn "++) . goLam t)
      Let t u -> ("let "++) . go False t . ("\n;\n"++) . go False u

