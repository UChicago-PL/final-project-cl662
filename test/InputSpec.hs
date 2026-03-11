-- test/InputSpec.hs
module InputSpec (spec) where

import Test.Hspec
import UI.Input (parsePos)

spec :: Spec
spec = do
  describe "parsePos" $ do
    it "parses comma-separated" $
      parsePos "1,2" `shouldBe` Right (1,2)

    it "parses space-separated" $
      parsePos "1 2" `shouldBe` Right (1,2)

    it "parses parens" $
      parsePos "(1, 2)" `shouldBe` Right (1,2)

    it "rejects junk" $
      parsePos "abc" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False