{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Main (
  runMain,
  runReactMain
) where



import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map               as Map
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified LN.UI.App.Counter      as Counter
import qualified LN.UI.App.About        as About
import qualified LN.UI.App.Home         as Home
import           LN.UI.Router.Internal
import qualified LN.UI.Router.Tabbed    as Tabbed
import           LN.UI.Router.Types
import           React.Flux



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do
  liftIO $ print "runReactMain"
  -- let apps =
  --           [ aboutApp "About"
  --           , homeApp "Home"
  --           , counterApp "Counter"
  --           ]
  -- appViews <- mapM initApp apps
  let tabs = tabApp "tabs" -- appsToTabs "main tabs" apps appViews
  tabView <- initApp tabs
  case tabs of
    App{appRouter = Just ar} -> initRouter ar
    _                        -> pure ()
  reactRender "ln" tabView Nothing

  -- where
  -- appsToTabs tabsName apps appViews =
  --   tabApp tabsName $
  --     zipWith (\a v ->
  --       Tabbed.Tab (appName a) (\pr -> view v pr mempty)) -- (appRouter a))
  --     apps appViews
  -- where
  -- appsToTabs tabsName apps appViews =
  --   tabApp tabsName $
  --     zipWith (\a v ->
  --       Tabbed.Tab (appName a) (\pr -> view v pr mempty) (appRouter a))
  --     apps appViews



-- aboutApp :: Text -> App Tabbed.ParentRouter
-- aboutApp name = App name About.store (\st _ -> About.view_ st) About.AboutAction Nothing



-- homeApp :: Text -> App Tabbed.ParentRouter
-- homeApp name = App name Home.store (\st _ -> Home.view_ st) Home.HomeAction Nothing



aboutApp :: App ()
aboutApp = App "about-app" About.store (\st _ -> About.view_ st) About.AboutAction Nothing



homeApp :: App ()
homeApp = App "home-app" Home.store (\st _ -> Home.view_ st) Home.HomeAction Nothing



counterApp :: Text -> App Tabbed.ParentRouter
counterApp name = App name Counter.store (\st _ -> Counter.view_ st) Counter.CounterInit Nothing



tabApp :: Text -> App Tabbed.ParentRouter
tabApp name =
  let rst = Tabbed.newStore Tabbed.defaultTabbedState { Tabbed.tsAboutApp = aboutApp, Tabbed.tsHomeApp = homeApp }
  in
    App {
      appName       = name,
      appState      = rst,
      appView       = (\st rt -> Tabbed.view_ rst rt st),
      appInitAction = Tabbed.R_Home,
      appRouter     = (Just $ storeRouter rst)
    }

-- tabApp :: Text -> [Tabbed.Tab] -> App Tabbed.ParentRouter
-- tabApp name tabs =
--   let rst = Tabbed.newStore tabs
--   in
--     App name rst (\st rt -> Tabbed.view_ rst rt st) Tabbed.TabbedInit (Just $ storeRouter rst)
