{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( maskSecretChar
    , maskSecretWord
    , maskSecretSentence
    , isUnMasked
    , mkGame
    , play
    , isAlreadyGuessed
    ) where

import Data.Char (toUpper)
import Data.List (intercalate)
import Stash (secretStash)
import System.Random
import Control.Monad.State
import System.Console.ANSI

{-
read words from file
track quessed letters
limit amount of quesses


-}

type GuessedChars = String
type SecretWord = String
type SecretSentence = String
type MaskedSecretWord = String
type Tries = Int

data Game = Game
    {_secretSentence::SecretSentence
    ,_quessedLetters::GuessedChars
    } deriving Show



-- if char not in quessed chars hide it
maskSecretChar :: Char -> GuessedChars -> Char
maskSecretChar secretChar quesses = if (elem (toUpper secretChar) quesses) then secretChar else '_' 


maskSecretWord :: SecretWord -> GuessedChars -> MaskedSecretWord
maskSecretWord secretWord quesses = map (`maskSecretChar` (map toUpper quesses)) secretWord

-- same as maskSecretWord but keep " " unmasked
maskSecretSentence :: SecretSentence -> GuessedChars -> MaskedSecretWord
maskSecretSentence secretSentence quesses =  intercalate " " $ (`maskSecretWord` quesses) <$> (words secretSentence)

isUnMasked :: MaskedSecretWord -> Bool
isUnMasked maskedWord = if (elem '_' maskedWord) then False else True 

getMaskedSentence :: Game -> String
getMaskedSentence game = maskSecretSentence (_secretSentence game) (_quessedLetters game)

getTries :: Game -> Int
getTries game = length $ _quessedLetters game

isAlreadyGuessed :: Game -> Char -> Bool
isAlreadyGuessed game guess = elem guess $ _quessedLetters game


isGameSolved :: Game -> Bool
isGameSolved game = isUnMasked ( getMaskedSentence game )


recordGuess :: (MonadIO m, MonadState Game m) => Char -> m ()
recordGuess guess = do
    game <- get
    let guesses = _quessedLetters game
    put game { _quessedLetters = guesses ++ [guess] }


mkGame :: IO Game
mkGame = do
    secretIndex <- randomRIO (0, length secretStash)
    let secret = secretStash !! secretIndex in
        return $ Game 
        { _secretSentence=secret
        , _quessedLetters=""
        }

renderGame :: (MonadIO m, MonadState Game m) => m ()
renderGame = do
    game   <- get
    liftIO $ do
        clearScreen
        putStrLn $ "Tries: " ++ (show $ getTries game)
        putStrLn $ "Previous guesses: " ++ "'" ++ ( _quessedLetters game) ++ "'"
        putStrLn $ "Your clue: " ++ getMaskedSentence game
        return ()

play ::  (MonadIO m, MonadState Game m) => m ()
play = do
    renderGame
    -- get the game to update the guess
    liftIO $ putStrLn "What is your next guess ?:"
    guess <- liftIO $ getChar
    liftIO $ putStrLn $ "guess: " ++ [guess]
    recordGuess guess
    game   <- get        
    if getTries game < 10 then
        play
    else
        liftIO $ putStrLn "Sorry tries exceeded..."
        -- return ()
        
        
        