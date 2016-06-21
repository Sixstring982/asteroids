import Test.Hspec

import Math

main :: IO ()
main = hspec $ do
  describe "Math.degFromRad" $ do
    it "Converts radians to degrees" $ do
      degFromRad 0.0 `shouldBe` 0.0
