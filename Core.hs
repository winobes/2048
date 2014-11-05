module Core where

import Data.List (findIndices, transpose)

type Tile = Int
type Score = Int
type Board = [[Tile]]
data Move = L | R | U | D

pile :: [Tile] -> [Tile] 
pile r = tiles ++ spaces 
  where tiles  = filter (/=0) r
        spaces = filter (==0) r 

merge :: Score -> [Tile] -> ([Tile], Score) 
merge n (x : 0 : rest) = let (rest', n') = merge n rest in (x:0:rest', n')
merge n (x : y : rest)
  | x == y    = let (rest', n') = merge n rest     in ((x+y):rest' ++ [0], n'+x+y)
  | otherwise = let (rest', n') = merge n (y:rest) in (x:rest', n')
merge n rest = (rest, n)

pileMerge :: [Tile] -> ([Tile], Score) 
pileMerge r = merge 0 (pile r)

isValid :: Move -> Board -> Bool
isValid m b = b' /= b
  where (b', _) = move m b

move :: Move -> Board  -> (Board, Score)
move L b = let (b', as) = unzip $ map pileMerge b in (b', sum as)
move R b = let (b', a)  = move L (map reverse b)  in (map reverse b', a)
move U b = let (b', a)  = move L (transpose b)    in (transpose b', a)
move D b = let (b', a)  = move R (transpose b)    in (transpose b', a)

placeTile :: Tile -> (Int, Int) -> Board -> Board
placeTile t (i, 0) (r:rs) = (inRow t i r) : rs
  where inRow t 0 (x:xs) = t:xs
        inRow t i (x:xs) = x : inRow t (i-1) xs
placeTile t (i, j) (r:rs) = r : placeTile t (i, j-1) rs
placeTile _ _ [] = error "invalid row index"

emptyCells :: Board -> [(Int, Int)]
emptyCells b = concat [inRow i [] (b!!i) | i <- [0..3]]
  where inRow i cs xs = map (\j -> (i,j)) (findIndices (==0) xs)

emptyBoard :: Board
emptyBoard = replicate 4 (replicate 4 0)
