{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.Class.CRUD2Spec (
  main,
  spec
) where



import           LN.UI.Router.Class.CRUD2
import           Test.Hspec
import Web.Routes



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "crud tests" $ do
    it "toPathInfo should work" $ do
      toPathInfo Index          `shouldBe` "/"
      toPathInfo (ShowS "ln")   `shouldBe` "/ln"
      toPathInfo (ShowI 9)      `shouldBe` "/9"
      toPathInfo (ShowB True)   `shouldBe` "/true"
      toPathInfo (ShowB False)  `shouldBe` "/false"
      toPathInfo New            `shouldBe` "/new"
      toPathInfo (EditS "ln")   `shouldBe` "/_edit/ln"
      toPathInfo (EditI 9)      `shouldBe` "/_edit/9"
      toPathInfo (DeleteS "ln") `shouldBe` "/_delete/ln"
      toPathInfo (DeleteI 9)    `shouldBe` "/_delete/9"
      toPathInfo DeleteZ        `shouldBe` "/delete"

