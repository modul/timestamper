import Test.Hspec
import Stamp


main :: IO ()
main = hspec $ do
    describe "addToFilePath Before" $ do
        it "adds a string in front of a file name" $ do
            addToFilePath Before "Foo" "test.txt" `shouldBe` "Foo-test.txt"
        it "adds a string in front of a directory name" $ do
            addToFilePath Before "Foo" "to/" `shouldBe` "Foo-to"
        it "adds a string in front of a directory path" $ do
            addToFilePath Before "Foo" "/path/to/" `shouldBe` "/path/Foo-to"
        it "adds a string in front of a file path" $ do
            addToFilePath Before "Foo" "/path/to/test.txt" `shouldBe` "/path/to/Foo-test.txt"
        it "handles relative paths" $ do
            addToFilePath Before "Foo" "../path/test.txt" `shouldBe` "../path/Foo-test.txt"
            addToFilePath Before "Foo" "../test.txt" `shouldBe` "../Foo-test.txt"
        it "sanitizes paths" $ do
            addToFilePath Before "Foo" "////test.txt" `shouldBe` "/Foo-test.txt"
    describe "addToFilePath After" $ do
        it "adds a string at the end of a file name" $ do
            addToFilePath After "Foo" "test.txt" `shouldBe` "test-Foo.txt"
        it "adds a string at the end of a directory name" $ do    
            addToFilePath After "Foo" "to/" `shouldBe` "to-Foo"
        it "adds a string at the end of a directory path" $ do    
            addToFilePath After "Foo" "/path/to/" `shouldBe` "/path/to-Foo"
        it "adds a string at the end of a file path" $ do
            addToFilePath After "Foo" "/path/to/test.txt" `shouldBe` "/path/to/test-Foo.txt"
        it "handles relative paths" $ do
            addToFilePath After "Foo" "../path/test.txt" `shouldBe` "../path/test-Foo.txt"
            addToFilePath After "Foo" "../test.txt" `shouldBe` "../test-Foo.txt"
        it "sanitizes paths" $ do
            addToFilePath After "Foo" "////test.txt" `shouldBe` "/test-Foo.txt"
