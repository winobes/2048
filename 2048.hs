module Game2048 where

import Data.List (findIndices)
import System.Random

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

putTile :: Tile -> Board -> (Int, Int) -> Board
putTile x b (i, j) = putRow i (update j x (b!!i)) b

-- The modify functions take a function that operates on a list of tiles 
-- and uniformly applies it to all of the rows/colums of the board.

modifyRow :: ([Tile] -> [Tile]) -> Board -> Board
modifyRow f b = foldr (\i b -> putRow i (f (b!!i)) b) b [0..3]

modifyCol :: ([Tile] -> [Tile]) -> Board -> Board
modifyCol f b = foldr (\i b -> putCol i (f [b!!j!!i | j <- [0..3]]) b) b [0..3]

pileL :: [Tile] -> [Tile]
pileL (Nothing:r) = pileL r ++ [Nothing]
pileL (x:r)       = x : pileL r
pileL []          = []

pileR :: [Tile] -> [Tile]
pileR (x:y:z:Nothing:r) = Nothing : pileR (x:y:z:r)
pileR (x:y:Nothing:r)   = Nothing : pileR (x:y:r)
pileR (x:Nothing:r)     = Nothing : pileR (x:r)
pileR (Nothing:r)       = Nothing : pileR r
pileR (x:r)             = x       : pileR r
pileR []                = []

combineL :: [Tile] -> [Tile]
combineL (Just x : Just y : rest) 
  | x == y    = Just (x+1) : combineL rest ++ [Nothing]
  | otherwise =  Just x : combineL (Just y : rest)
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

randomPick :: [a] -> IO a
randomPick xs = fmap ((!!) xs) i
  where i = getStdRandom (randomR (0, l))
        l = (length xs) - 1

randomPlace :: Board -> IO Board
randomPlace b = fmap (putTile (Just 1) b) cell
  where cell = randomPick (emptyCells b)

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

main :: IO ()
main = do 
  let board = initBoard
  gameLoop (return board)
  putStrLn "Done."

gameLoop :: IO Board -> IO Board
gameLoop board = do
  fmap showBoard board >>= putStr
  if False 
    then board
    else do
           c <- getChar
           putStrLn $ ""
           case c of
             'A' -> gameLoop $ (fmap goU) board >>= randomPlace
             'B' -> gameLoop $ (fmap goD) board >>= randomPlace
             'C' -> gameLoop $ (fmap goR) board >>= randomPlace
             'D' -> gameLoop $ (fmap goL) board >>= randomPlace
             _   -> gameLoop board

