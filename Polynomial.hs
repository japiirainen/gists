{-| Experimenting with representing polynomials in Haskell.
-}

{-# LANGUAGE RecordWildCards #-}

module Polynomial where

import Data.List (intersperse, foldl')
import Data.Map (Map)
import Data.Bifunctor (bimap)
import Control.Monad (join)

import qualified Data.Map as Map
import qualified Data.Set as Set

data Polynomial = Polynomial
  { indeterminate :: Char
  , coefficients :: [Float]
  }

coes :: (Polynomial, Polynomial) -> ([Float], [Float])
coes = (bimap coefficients coefficients)

instance Show Polynomial where
  show Polynomial{..} = join $ intersperse " + " $
      map f (zip coefficients [0..])
    where
      f (coe, 0) = show coe
      f (coe, idx) = show coe <> [indeterminate] <> "^" <> show idx

poly :: [Float] -> Polynomial
poly = Polynomial 'x'

zero :: Polynomial
zero = poly []

-- | Evaluate a polynomial at an input point. Uses Horner's method.
eval :: Polynomial -> Float -> Float
eval Polynomial{..} x = foldl' (\acc c -> acc * x + c) 0 (reverse coefficients)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) f g a b = f $ g a b
infixl 8 ...

polyadd :: Num a => [a] -> [a] -> [a]
polyadd [] ys = ys
polyadd xs [] = xs
polyadd (x:xs) (y:ys) = (x+y) : polyadd xs ys

polyscale :: Num a => a -> [a] -> [a]
polyscale a = map (a*)

polymult :: Num a => [a] -> [a] -> [a]
polymult ys = foldr (\x acc -> polyadd (polyscale x ys) (0 : acc)) []

instance Num Polynomial where
  (+) = poly . uncurry polyadd . coes ... (,)
  (*) = poly . uncurry polymult . coes ... (,)
  negate = poly . (map negate) . coefficients
  abs = poly . (map abs) . coefficients
  signum = poly . (map signum) . coefficients
  fromInteger x = poly [fromInteger x]

type Point = (Float, Float)

singleTerm :: [Point] -> Int -> Polynomial
singleTerm points i = theTerm * poly [yi]
  where
    (xi, yi) = points !! i
    f acc (p, j) = if j == i then acc else
      let xj = (fst p) in acc * poly [-xj / (xi - xj), 1 / (xi - xj)]
    theTerm = foldl' f (poly (pure 1)) (zip points [0..])

-- | Return the unique polynomial of degree at most n passing through the given n+1 points.
interpolate :: [Point] -> Polynomial
interpolate [] = error "Must provide at least one point."
interpolate xs
  | length (map fst xs) > Set.size (Set.fromList (map fst xs)) = error "Not all x values are distinct."
  | otherwise = foldl' (+) zero terms
    where
      terms = map (singleTerm xs) [0..(length xs)-1]

main :: IO ()
main = do
  -- f(x) = 1 + 2x + 3x^2
  let f = poly [1, 2, 3]
  -- -- g(x) = -8 + 17x + 5x^3
  let g = poly [-8, 17, 0, 5]

  -- print $ uncurry zip $ coes $ fillZeros f g
  print $ f + g
  print $ f * g
  print $ eval f 1
  print $ eval (f * g) 3

  print $ interpolate [(1, 1), (2, 0)]
  print $ interpolate [(1, 1), (2, 4), (7, 9)]

