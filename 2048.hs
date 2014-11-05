module Main where

import Data.Maybe (isNothing, fromJust)
import Data.List (findIndices, transpose)
import System.Random
import System.IO

type Tile = Int
type Board = [[Tile]]
data Move = L | R | U | D

pile :: [Tile] -> [Tile] 
pile r = tiles ++ spaces 
  where tiles  = filter (/=0) r
        spaces = filter (==0) r 

merge :: Int -> [Tile] -> ([Tile], Int) 
merge n (x : 0 : rest) = let (rest', n') = merge n rest in (x:0:rest', n')
merge n (x : y : rest)
  | x == y    = let (rest', n') = merge n rest     in ((x+y):rest' ++ [0], n'+x+y)
  | otherwise = let (rest', n') = merge n (y:rest) in (x:rest', n')
merge n rest = (rest, n)

pileMerge :: [Tile] -> ([Tile], Int) 
pileMerge r = merge 0 (pile r)

isValid :: Move -> Board -> Bool
isValid m b = (map pile b) /= b

move :: Move -> Board  -> (Board, Int)
move L b = let (b', as) = unzip $ map pileMerge b in (b', sum as)
move R b = let (b', a)  = move L (map reverse b)  in (map reverse b', a)
move U b = let (b', a)  = move R (transpose b)    in (transpose b', a)
move D b = let (b', a)  = move L (transpose b)    in (transpose b', a)

emptyCells :: Board -> [(Int, Int)]
emptyCells b = concat [inRow i [] (b!!i) | i <- [0..3]]
  where inRow i cs xs = map (\j -> (i,j)) (findIndices (==0) xs)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick [] _ = error "picking from empty list"
randomPick xs g = (xs!!i, g')
  where (i, g') = randomR (0, l) g
        l = (length xs) - 1

placeTile :: Tile -> (Int, Int) -> Board -> Board
placeTile t (i, 0) (r:rs) = (inRow t i r) : rs
  where inRow t 0 (x:xs) = t:xs
        inRow t i (x:xs) = x : inRow t (i-1) xs
placeTile t (i, j) (r:rs) = r : placeTile t (i, j-1) rs
placeTile _ _ [] = error "invalid row index"

randomPlace :: RandomGen g => Board -> g -> (Board, g)
randomPlace b g = ((placeTile tile cell b), g'')
  where (cell, g')  = randomPick (emptyCells b) g
        (tile, g'') = randomPick [2, 4] g'

emptyBoard :: Board
emptyBoard = replicate 4 (replicate 4 0)

showBoard :: Board -> String
showBoard b = 
      "+----+----+----+----+\n"
   ++ showRow (b!!0)   ++ "|\n"
   ++ "+----+----+----+----+\n"
   ++ showRow (b!!1)   ++ "|\n"
   ++ "+----+----+----+----+\n"
   ++ showRow (b!!2)   ++ "|\n"
   ++ "+----+----+----+----+\n"
   ++ showRow (b!!3)   ++ "|\n"
   ++ "+----+----+----+----+\n"

showRow :: [Tile] -> String 
showRow r = "|" ++ showTile (r!!0)
         ++ "|" ++ showTile (r!!1)
         ++ "|" ++ showTile (r!!2)
         ++ "|" ++ showTile (r!!3)

showTile :: Tile -> String
showTile 0  = "    "
showTile n = (replicate i ' ') ++ s
  where s = show n
        i = 4 - (length s)

data GameState = GS { board :: Board
                    , score :: Int
                    , g     :: StdGen
                    }

main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering
  let (b, g) = randomPlace emptyBoard (mkStdGen 14)
      gS = GS b 0 g
  updateGraphics gS
  gameLoop gS

gameLoop :: GameState -> IO ()
gameLoop gS = do
  c <- return R 
  let gS' = updateBoard c gS 
  updateGraphics gS'
  if False
    then putStrLn "Game Over."
    else gameLoop gS'

updateGraphics :: GameState -> IO ()
updateGraphics gs = do putStr $ "\n" ++ show (score gs) ++ "\n" ++ showBoard (board gs)
                      
updateBoard :: Move -> GameState -> GameState 
updateBoard m gs@(GS b s g) = GS b'' (s+s') g'
  where (b', s')  = move m b
        (b'', g') = randomPlace b' g
