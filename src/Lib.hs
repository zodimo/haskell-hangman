module Lib
    ( maskSecretChar
    , maskSecretWord
    , maskSecretSentence
    , isUnMasked
    , getSecretWord
    ) where

import Data.Char (toUpper)
import Data.List (intercalate)
{-
read words from file
track quessed letters
limit amount of quesses


-}

type GuessedChars = String
type SecretWord = String
type SecretSentence = String
type MaskedSecretWord = String


-- if char not in quessed chars hide it
maskSecretChar :: Char -> String -> Char
maskSecretChar secretChar quesses = if (elem (toUpper secretChar) quesses) then secretChar else '_' 


maskSecretWord :: SecretWord -> GuessedChars -> MaskedSecretWord
maskSecretWord secretWord quesses = map (`maskSecretChar` (map toUpper quesses)) secretWord

-- same as maskSecretWord but keep " " unmasked
maskSecretSentence :: SecretSentence -> GuessedChars -> MaskedSecretWord
maskSecretSentence secretSentence quesses =  intercalate " " $ (`maskSecretWord` quesses) <$> (words secretSentence)

isUnMasked :: MaskedSecretWord -> Bool
isUnMasked maskedWord = if (elem '_' maskedWord) then False else True 

getSecretWord :: IO String
getSecretWord = do
    putStrLn "Please enter the secret word:"
    getLine

