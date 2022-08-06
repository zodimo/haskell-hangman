module Main where
import Control.Monad.State
import Lib


main :: IO ()
main = do
    game    <- mkGame
    runStateT play game
    return ()
    
