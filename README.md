# Haskell Hangman (Guess the blockchain phrase)
This is a hangman-like "guess-the-phrase" one letter at a time game.  
The theme of the phrases to guess are blockchain.  
  
The secret phrase can be one or more words. Each word will be displayed as a series of "_" representing each letter to be guessed. Words are separated by a single space. (" ")    

The game is played by using the keyboard to enter a one key a-z at a time to guess the word. When a letter is present in the prase all the places the character exist will be shown.  

e.g "Private Key" with the letter "e" guessed will be displayed as : 
```text
______e _e_
```

The game will end then all the letters in the phrase are guessed.  


