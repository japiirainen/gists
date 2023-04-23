module Main where

import Data.List (foldl', partition, transpose, (\\))

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char
type Choices = [Value]

boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1' .. '9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

-- Example grids
-------------

-- Solvable only using the basic rules:
easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

-- First gentle example from sudoku.org.uk:
gentle :: Grid
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

-- First diabolical example:
diabolical :: Grid
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

-- First "unsolvable" (requires backtracking) example:
unsolvable :: Grid
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
  ]

-- Minimal sized grid (17 values) with a unique solution:
minimal :: Grid
minimal =
  [ ".98......",
    "....7....",
    "....15...",
    "1........",
    "...2....9",
    "...9.6.82",
    ".......3.",
    "5.1......",
    "...4...2."
  ]

-- An empty grid.
emptyGrid :: Grid
emptyGrid = replicate 9 ".........."

rows, cols, boxes :: Matrix a -> [Row a]
rows = id
cols = transpose
boxes = unpack . map cols . pack
  where
    pack = split . map split
    split = chop boxsize
    unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Eq a => Matrix a -> Bool
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxes g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = x `notElem` xs && nodups xs

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where
    choice v = if empty v then values else [v]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xs =
  let singles = concat . filter single
   in map (\ys -> if single ys then ys else ys \\ singles xs) xs

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let x' = f x
   in if x == x' then x else fix f x'

-- This can solve easy sudoku puzzles,
-- but never terminates for harder ones since after pruning there can still be ~10^29 possibilities.
solveEasy :: Grid -> [Grid]
solveEasy = filter valid . collapse . fix prune . choices

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe m =
  all consistent (rows m)
    && all consistent (cols m)
    && all consistent (boxes m)

consistent :: Row Choices -> Bool
consistent = nodups . concat . filter single

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

solve :: Grid -> [Grid]
solve = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | all (all single) m = collapse m
  | otherwise =
    [g | m' <- expand m, g <- search (prune m')]

expand :: Matrix Choices -> [Matrix Choices]
expand m =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = span (all single) m
    (row1, cs : row2) = span single row

main :: IO ()
main = putStrLn (unlines (head (solve minimal)))
