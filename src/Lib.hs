{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( maskSecretChar
    , maskSecretWord
    , maskSecretSentence
    , isUnMasked
    , mkGame
    , play
    ) where


import Data.Char (toUpper)
import Data.List (intercalate)
import Stash (secretStash)
import System.Random
import Control.Monad.State
import System.Console.ANSI
import System.IO


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

getTries :: Game -> Tries
getTries game = length $ _quessedLetters game

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
        putStrLn $ "== Blockchain theme guess the word hangman game =="
        putStrLn $ "Tries: " ++ (show $ getTries game)
        putStrLn $ "Previous guesses: " ++ "'" ++ ( _quessedLetters game) ++ "'"
        putStrLn $ "Your clue: " ++ getMaskedSentence game
        return ()

play ::  (MonadIO m, MonadState Game m) => m ()
play = do
    renderGame
    -- get the game to update the guess
    liftIO $ putStrLn "What is your next guess ?:"
    {-
        BUG with getChar..
        https://www.reddit.com/r/haskellquestions/comments/egz4ic/getchar_doesnt_work_until_i_press_the_enter_key/
    -}
    liftIO $ hSetBuffering stdin NoBuffering
    guess <- liftIO $ getChar
    liftIO $ putStrLn $ "guess: " ++ [guess]
    recordGuess guess
    game   <- get        
    if isGameSolved game  then
        liftIO $ do
            clearScreen
            putStrLn $ "Welldone you did it in " ++ (show $ getTries game) ++ " tries!"
            putStrLn $ "The secret word(s) was: '" ++ (_secretSentence game) ++ "'"
    else
        play
        
        
        
        