{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( maskSecretChar,
    maskSecretWord,
    maskSecretSentence,
    isUnMasked,
    mkGame,
    play,
  )
where

import Control.Monad.State
import Data.Char (toUpper)
import Data.List (intercalate)
import Stash (secretStash)
import System.Console.ANSI
import System.IO
import System.Random

type GuessedChars = String

type SecretWord = String

type SecretSentence = String

type MaskedSecretWord = String

type Tries = Int

data Game = Game
  { _secretSentence :: SecretSentence,
    _quessedLetters :: GuessedChars
  }
  deriving (Show)

-- if character is correctly guessed then unmask else mask with _
maskSecretChar :: Char -> GuessedChars -> Char
maskSecretChar secretChar quesses = if toUpper secretChar `elem` quesses then secretChar else '_'

-- unmask characters from secret word correctly guessed. (case-insensitive)
maskSecretWord :: SecretWord -> GuessedChars -> MaskedSecretWord
maskSecretWord secretWord quesses = map (`maskSecretChar` map toUpper quesses) secretWord

-- make one or more words
maskSecretSentence :: SecretSentence -> GuessedChars -> MaskedSecretWord
maskSecretSentence secretSentence quesses = unwords $ (`maskSecretWord` quesses) <$> words secretSentence

-- test if word contains masked characters
isUnMasked :: MaskedSecretWord -> Bool
isUnMasked maskedWord = '_' `notElem` maskedWord

-- mask secret from game state
getMaskedSentence :: Game -> String
getMaskedSentence game = maskSecretSentence (_secretSentence game) (_quessedLetters game)

-- amount of guesses is the amount of tries
getTries :: Game -> Tries
getTries game = length $ _quessedLetters game

-- if secret is unmasked then the game is solved.
isGameSolved :: Game -> Bool
isGameSolved game = isUnMasked (getMaskedSentence game)

-- append the current guess to the list of guessed characters and save to the game state
recordGuess :: (MonadState Game m) => Char -> m ()
recordGuess guess = do
  game <- get
  let guesses = _quessedLetters game
  put game {_quessedLetters = guesses ++ [guess]}

-- game init
mkGame :: IO Game
mkGame = do
  secretIndex <- randomRIO (0, length secretStash)
  let secret = secretStash !! secretIndex
   in return $
        Game
          { _secretSentence = secret,
            _quessedLetters = ""
          }

-- render game state
renderGame :: (MonadIO m, MonadState Game m) => m ()
renderGame = do
  game <- get
  liftIO $ do
    clearScreen
    putStrLn "== Blockchain theme guess the word hangman game =="
    putStrLn $ "Tries: " ++ show (getTries game)
    putStrLn $ "Previous guesses: " ++ "'" ++ _quessedLetters game ++ "'"
    putStrLn $ "Your clue: " ++ getMaskedSentence game
    return ()

-- game loop
play :: (MonadIO m, MonadState Game m) => m ()
play = do
  renderGame
  liftIO $ putStrLn "What is your next guess ?:"
  {-
      BUG with getChar..
      https://www.reddit.com/r/haskellquestions/comments/egz4ic/getchar_doesnt_work_until_i_press_the_enter_key/
  -}
  liftIO $ hSetBuffering stdin NoBuffering
  guess <- liftIO getChar
  liftIO $ putStrLn $ "guess: " ++ [guess]
  recordGuess guess
  game <- get
  if isGameSolved game
    then liftIO $ do
      clearScreen
      putStrLn $ "Welldone you did it in " ++ show (getTries game) ++ " tries!"
      putStrLn $ "The secret word(s) was: '" ++ _secretSentence game ++ "'"
    else play