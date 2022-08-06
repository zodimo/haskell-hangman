import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "maskSecretWord" $ do
        it "mask chars not yet guessed in secret word " $ do
            maskSecretWord "Eiffel" "e" ` shouldBe` "E___e_"