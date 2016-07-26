{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.RouteSpec (
  main,
  spec
) where



import           LN.T.Param
import           LN.UI.Router.CRUD
import           LN.UI.Router.Param
import           LN.UI.Router.Route
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
      fromPathInfo "/ln"                       `shouldBe` (Right $ Organizations (ShowS "ln"))
      fromPathInfo "/organizations/_edit/ln"   `shouldBe` (Right $ Organizations (EditS "ln"))
      fromPathInfo "/organizations/_delete/ln" `shouldBe` (Right $ Organizations (DeleteS "ln"))


  describe "route with tests" $ do
    it "toRouteWith should gives us a proper RouteWith" $ do
      toRouteWith "/about"
        `shouldBe` (RouteWith About emptyParams)

      toRouteWith "/about?limit=1&offset=2"
        `shouldBe` (RouteWith About $ buildParams [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])

    it "fromRouteWith should give us a proper url string" $ do
      fromRouteWith (routeWith' About)
        `shouldBe` "/about"

      fromRouteWith (routeWith About [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])
        `shouldBe` "/about?limit=1&offset=2"

    --
    -- HASHES
    --

    it "toRouteWithHash should gives us a proper RouteWith" $ do
      toRouteWithHash "#/about"
        `shouldBe` (RouteWith About emptyParams)

      toRouteWithHash "#/about?limit=1&offset=2"
        `shouldBe` (RouteWith About $ buildParams [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])

    it "fromRouteWithHash should give us a proper url string" $ do
      fromRouteWithHash (routeWith' About)
        `shouldBe` "#/about"

      fromRouteWithHash (routeWith About [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])
        `shouldBe` "#/about?limit=1&offset=2"
