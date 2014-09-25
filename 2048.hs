module Main where

import Data.List (findIndices)
import Data.Maybe (fromJust, isJust)
import Control.Monad (when)
import System.Random
import System.IO

type Tile = Maybe Int

type Board = [[Tile]]

validIndex :: Int -> Bool
validIndex i = i < 4 && i > -1 

update :: Int -> a -> [a] -> [a]
update _ _ [] = []
update 0 y (x:xs) = y:xs
update n y (x:xs) = x : update (n - 1) y xs

putRow :: Int -> [Tile] -> Board -> Board
putRow i r b | validIndex i = update i r b
             | otherwise = error "invalud row"

putCol :: Int -> [Tile] -> Board -> Board
putCol i c b | validIndex i = [update i (c!!j) (b!!j) | j <- [0..3]]
             | otherwise = error "invalud column"

putTile :: Tile -> (Int, Int) -> Board -> Board
putTile x (i, j) b = putRow i (update j x (b!!i)) b

-- The modify functions take a function that operates on a list of tiles 
-- and uniformly apply it to all of the rows/colums of a board.

modifyRow :: ([Tile] -> [Tile]) -> Board -> Board
modifyRow f b = foldr (\i b -> putRow i (f (b!!i)) b) b [0..3]

modifyCol :: ([Tile] -> [Tile]) -> Board -> Board
modifyCol f b = foldr (\i b -> putCol i (f [b!!j!!i | j <- [0..3]]) b) b [0..3]

pileL :: [Tile] -> [Tile]
pileL xs = tiles ++ replicate n Nothing
  where tiles = filter (isJust) xs  
        n = 4 - length tiles

pileR :: [Tile] -> [Tile]
pileR xs =  replicate n Nothing ++ tiles
  where tiles = filter (isJust) xs
        n = 4 - length tiles

combineL :: [Tile] -> [Tile]
combineL (Just x : Just y : rest) 
  | x == y    = Just (x+1) : combineL rest ++ [Nothing]
  | otherwise = Just x : combineL (Just y : rest)
combineL (x : rest) = x : combineL rest
combineL [] = []

combineR :: [Tile] -> [Tile]
combineR = reverse . combineL . reverse

goL :: Board -> Board
goL = modifyRow (combineL . pileL)
 
goU :: Board -> Board
goU = modifyCol (combineL . pileL)

goR :: Board -> Board
goR = modifyRow (combineR . pileR) 

goD :: Board -> Board
goD = modifyCol (combineR . pileR) 

emptyCells :: Board -> [(Int, Int)]
emptyCells b = concat [go i [] (b!!i) | i <- [0..3]]
  where go i cs xs = map (\j -> (i,j)) (findIndices (==Nothing) xs)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPck [] _ = error "picking from empyt list"
randomPick xs g = (xs!!i, g')
  where (i, g') = randomR (0, l) g
        l = (length xs) - 1

randomPlace :: RandomGen g => Board -> g -> (Board, g)
randomPlace b g = ((putTile tile cell b), g'')
  where (cell, g')  = randomPick (emptyCells b) g
        (tile, g'') = randomPick [Just 1, Just 2] g'

initBoard :: Board
initBoard = replicate 4 (replicate 4 Nothing)

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
showTile Nothing  = "    "
showTile (Just n) = (replicate i ' ') ++ s
  where s = show (2^n)
        i = 4 - (length s)

data GameState = GS Board Int StdGen Bool

main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering
  let (b, g) = randomPlace initBoard (mkStdGen 14)
      gS = GS b 0 g True 
  updateGraphics gS
  gameLoop gS

gameLoop :: GameState -> IO ()
gameLoop gS = do
  c <- getChar
  let gS' = updateBoard c gS 
  updateGraphics gS'
  if win gS' || loose gS' 
    then putStrLn "Game Over."
    else gameLoop gS'

updateGraphics :: GameState -> IO ()
updateGraphics (GS board score g moved) = do
  when moved $ putStr $ "\n" ++ show score
                     ++ "\n" ++ showBoard board
                      
updateBoard :: Char -> GameState -> GameState 
updateBoard c (GS board score g _) | board == boardM = GS board score g  False 
                                   | otherwise       = GS board' score' g' True
  where boardM = move c board  
        score' = score + 1
        (board', g') = randomPlace boardM g

-- TODO: make winning possible
win :: GameState -> Bool
win (GS board _ _ _) = False

loose :: GameState -> Bool
loose (GS board _ _ _ ) = (null $ emptyCells board)
                    &&  all (\f -> f board == board) [goU, goD, goR, goL]

move :: Char -> (Board -> Board)
move 'A' = goU
move 'B' = goD
move 'C' = goR
move 'D' = goL
move  _  = id

