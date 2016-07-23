module LN.UI.App.PageNumbersSpec (
  main
) where



import LN.UI.App.PageNumbers
import Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "pageRange tests" $ do
    it "should provide a proper range" $ do
      pageRange 1 10 `shouldBe` [1..10]
