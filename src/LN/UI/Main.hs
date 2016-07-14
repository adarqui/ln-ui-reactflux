{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Main (
  runMain,
  runReactMain
) where



import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Monoid
import React.Flux
import qualified Data.Text as Text
import Data.Text (Text)
import qualified LN.UI.Router.Tabbed as Tabbed
import LN.UI.Router.Internal
import LN.UI.Router.Types
import qualified LN.UI.App.Counter as Counter



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do
  liftIO $ print "runReactMain"
  let apps = [counterApp "Counter", counterApp "Counter2", counterApp "counter3"]
  appViews <- mapM initApp apps
  let tabs = appsToTabs "main tabs" apps appViews
  tabView <- initApp tabs
  case tabs of
    App{appRouter = Just ar} -> initRouter ar
    _                        -> pure ()
  reactRender "ln" tabView Nothing
  where
  appsToTabs tabsName apps appViews =
    tabApp tabsName $
      zipWith (\a v ->
        Tabbed.Tab (appName a) (\pr -> view v pr mempty) (appRouter a))
      apps appViews



counterApp :: Text -> App Tabbed.ParentRouter
counterApp name = App name Counter.store (\st _ -> Counter.view_ st) Counter.CounterInit Nothing



tabApp :: Text -> [Tabbed.Tab] -> App Tabbed.ParentRouter
tabApp name tabs =
  let rst = Tabbed.newStore tabs
  in
    App name rst (\st rt -> Tabbed.view_ rst rt st) Tabbed.TabbedInit (Just $ storeRouter rst)
