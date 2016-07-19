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
      fromPathInfo "/"                         `shouldBe` (Right Home)
      fromPathInfo "/about"                    `shouldBe` (Right About)
      fromPathInfo "/me"                       `shouldBe` (Right Me)
      fromPathInfo "/errors"                   `shouldBe` (Right Errors)
      fromPathInfo "/portal"                   `shouldBe` (Right Portal)
      fromPathInfo "/organizations"            `shouldBe` (Right $ Organizations Index)
      fromPathInfo "/organizations/new"        `shouldBe` (Right $ Organizations New)
      fromPathInfo "/organizations/_edit/ln"   `shouldBe` (Right $ Organizations (EditS "ln"))
      fromPathInfo "/organizations/_delete/ln" `shouldBe` (Right $ Organizations (DeleteS "ln"))
