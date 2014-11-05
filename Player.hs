module Player where

import Core

class Player p where
  initPlayer   :: p
  updatePlayer :: p -> Board -> p
  getMove      :: p -> IO Move

data Human = Human
instance Player Human where
  initPlayer = Human
  updatePlayer _ _ = Human
  getMove p = do 
    c <- getChar
    case c of 'c' -> return U
              'h' -> return L
              't' -> return D
              'n' -> return R
              _   -> getMove p
