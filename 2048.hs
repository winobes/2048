module Main where

import Core
import Player
import System.Random 
import System.IO
import Control.Monad (liftM2)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick [] _ = error "picking from empty list"
randomPick xs g = (xs!!i, g')
  where (i, g') = randomR (0, l) g
        l = (length xs) - 1

randomPlace :: RandomGen g => Board -> g -> (Board, g)
randomPlace b g = ((placeTile tile cell b), g'')
  where (cell, g')  = randomPick (emptyCells b) g
        (tile, g'') = randomPick [2, 4] g'

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

main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering
  (b, g') <- (liftM2 randomPlace) (return emptyBoard) getStdGen 
  updateGraphics b 0 
  gameLoop Human b 0 g'

gameLoop :: Player p => p -> Board -> Score -> StdGen -> IO ()
gameLoop p b s g = do
  m  <- getMove p 
  if isValid m b
    then do (b', s', g') <- return $ updateBoard m b s g
            p'           <- return $ updatePlayer p b'
            updateGraphics b' s'
            gameLoop p' b' s' g'
    else do gameLoop p  b  s  g

updateGraphics :: Board -> Score -> IO ()
updateGraphics b s = do putStr $ "\n" ++ show s ++ "\n" ++ showBoard b 
                      
updateBoard :: Move -> Board -> Score -> StdGen -> (Board, Score, StdGen) 
updateBoard m b s g = (b'', (s+s'), g')
  where (b', s')  = move m b
        (b'', g') = randomPlace b' g
