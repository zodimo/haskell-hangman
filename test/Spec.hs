import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "maskSecretWord" $ do
        it "mask chars not yet guessed in secret word " $ do
            maskSecretWord "Eiffel" "e" ` shouldBe` "E___e_"

    -- sanity check on words
    describe "words" $ do
        it "single word is a list with 1 word" $ do
            words "word" `shouldBe` ["word"] 

    describe "fmap sentence" $ do
        it "mask words in a list" $ do
            fmap (`maskSecretWord` "o") (words "word") `shouldBe` ["_o__"] 
    
    describe "maskSecretPhrase" $ do
        it "mask chars not yet guessed in secret phrase " $ do
            maskSecretPhrase "Eiffel Tower" "e" ` shouldBe` "E___e_ ___e_"
