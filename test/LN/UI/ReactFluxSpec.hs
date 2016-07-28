{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFluxSpec (
  main,
  spec
) where



import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "empty test" $ do
    it "should pass" $ do
      True `shouldBe` True
