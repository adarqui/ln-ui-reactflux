module LN.UI.App.PageNumbersSpec (
  main,
  spec
) where



import           LN.UI.App.PageNumbers
import           LN.UI.Router.Route
import           LN.UI.State.PageInfo
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "pageRange tests" $ do
    it "should provide a proper range" $ do
      pageRange page_info  `shouldBe` []
      pageRange page_info2 `shouldBe` [1..totalPages page_info2]

  describe "buildPages tests" $ do
    it "should provide a proper Pages result" $ do
      buildPages page_info (routeWith' Home) `shouldBe` (1, [], 1, 20)

  where
  page_info  = defaultPageInfo
  page_info2 = page_info{ totalPages = 20 }
