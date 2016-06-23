import Test.Hspec

import Math

main :: IO ()
main = hspec $ do
  describe "Math.degFromRad" $ do
    it "Converts radians to degrees" $ do
      degFromRad 0.0 `shouldBe` 0.0
      degFromRad pi `shouldBe` 180

  describe "Math.digits" $ do
    it "Converts an int into a list of its digits" $ do
      digits 100 `shouldBe` [1, 0, 0]
      digits 1234 `shouldBe` [1, 2, 3, 4]
