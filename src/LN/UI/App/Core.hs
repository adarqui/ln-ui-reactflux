{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Core (
  Store (..),
  defaultStore,
  Action (..),
  store,
  view,
  view_,
  initRouter
) where


import           Control.Concurrent              (forkIO)
import           Control.DeepSeq                 (NFData)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Monoid                     ((<>))
import           Data.Rehtie                     (rehtie)
import           Data.Text                       (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           React.Flux                      hiding (view)
import qualified React.Flux                      as RF
import           React.Flux.Router.WebRoutes     (initRouterRaw'ByteString)

import           LN.Api                          (getMe')
import           LN.T.User                       (UserResponse (..))
import qualified LN.UI.App.About                 as About
import qualified LN.UI.App.Breadcrumbs           as Breadcrumbs
import qualified LN.UI.App.Home                  as Home
import qualified LN.UI.App.Organization          as Organization
import qualified LN.UI.App.Organizations         as Organizations
import qualified LN.UI.App.Portal                as Portal
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.ReactFluxDOM      (ahref, ahrefName)
import           LN.UI.Router.Class.App
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Route
import           LN.UI.State.PageInfo            (PageInfo, defaultPageInfo)



data Store = Store {
  _route    :: RouteWith,
  _me       :: Maybe UserResponse
} deriving (Typeable, Generic)

defaultStore :: Store
defaultStore = Store {
  _route    = routeWith' Home,
  _me       = Nothing
}



data Action
  = Init
  | SetRoute RouteWith
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do
    case action of
      Init           -> action_init
      SetRoute route -> action_set_route route
      _              -> pure st
    where
    action_init = do
      putStrLn "Init"
      lr <- rd getMe'
      rehtie lr (const $ pure st) $ \user_pack ->
        pure $ st{ _me = Just user_pack }

    action_set_route route_with@(RouteWith route params) = do

      putStrLn $ show route_with

      case route_with of

        RouteWith Home _                       -> pure ()
        RouteWith About _                      -> pure ()
        RouteWith Portal _                     -> pure ()
        RouteWith (Organizations Index) params -> void $ forkIO $ executeAction $ SomeStoreAction Organizations.store $ Organizations.Init route_with
        RouteWith (Users Index) params         -> pure ()
        RouteWith _ _                          -> pure ()

      pure $ st{ _route = route_with }



store :: ReactStore Store
store = mkStore defaultStore



view :: ReactView Store
view =
  defineControllerView "core" store $ \st _ ->
    defaultLayout st (renderRouteView st)



view_ :: Store  -> ReactElementM eventHandler ()
view_ st =
  RF.view view st mempty



initRouter :: IO ()
initRouter =
  initRouterRaw'ByteString (Just go) go
  where
  go = \raw_uri -> do
    print raw_uri
    routeAlterStore $ toRouteWithHash raw_uri
    where
      routeAlterStore action =
        -- Update Store with our new route
        liftIO $ alterStore store $ SetRoute action



defaultLayout :: Store -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
defaultLayout st@Store{..} page =
  div_ $ do
    navBar _me
    Breadcrumbs.view_ _route
    div_ page



navBar :: Maybe UserResponse -> ReactElementM ViewEventHandler ()
navBar m_user_pack =
  div_ $ do
    ul_ $ do
      li_ $ ahref $ routeWith' Home
      li_ $ ahref $ routeWith' About
      li_ $ ahref $ routeWith' Portal
      li_ $ do
        case m_user_pack of
          Nothing               -> ahref $ routeWith' Login
          Just UserResponse{..} -> ahrefName ("Logout: " <> userResponseName) $ routeWith' Logout



renderRouteView :: Store -> ReactElementM ViewEventHandler ()
renderRouteView Store{..} = do
  div_ $ do
    case _route of
      RouteWith Home _                        -> Home.view_
      RouteWith About _                       -> About.view_
      RouteWith Portal _                      -> Portal.view_
      RouteWith (Organizations Index) params  -> Organizations.view_
      RouteWith (Organizations crud) params   -> Organization.view_ crud
      RouteWith (Users Index) params          -> p_ $ elemText "Users Index"
      RouteWith (Users crud) params           -> p_ $ elemText "Users crud"
      RouteWith _ _                           -> p_ $ elemText "Unknown"
