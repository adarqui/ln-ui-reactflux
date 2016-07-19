{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.Class.Routes2Spec (
  main,
  spec
) where



import           LN.UI.Router.Class.CRUD2
import           LN.UI.Router.Class.Routes2
import           Test.Hspec
import           Web.Routes



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "routes tests" $ do
    it "toPathInfo should work" $ do
      toPathInfo Home                           `shouldBe` "/"
      toPathInfo About                          `shouldBe` "/about"
      toPathInfo Me                             `shouldBe` "/me"
      toPathInfo Errors                         `shouldBe` "/errors"
      toPathInfo Portal                         `shouldBe` "/portal"
      toPathInfo (Organizations Index)          `shouldBe` "/organizations"
      toPathInfo (Organizations New)            `shouldBe` "/organizations/new"
      toPathInfo (Organizations (ShowS "ln"))   `shouldBe` "/ln"
      toPathInfo (Organizations (EditS "ln"))   `shouldBe` "/organizations/_edit/ln"
      toPathInfo (Organizations (DeleteS "ln")) `shouldBe` "/organizations/_delete/ln"

    it "fromPathInfo should work" $ do
      fromPathInfo "/home"       `shouldBe` (Right Home)


--      >>> toPathInfo (BlogPost 123)
--      "/blog-post/123"
--      >>> fromPathInfo "/blog-post/123" :: Either String Sitemap
--      Right (BlogPost 123)
