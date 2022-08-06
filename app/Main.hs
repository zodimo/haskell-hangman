module Main where

import Lib

main :: IO ()
main = do
    secret <- getSecretWord
    putStrLn $ "The secret word is " ++ secret 
    
