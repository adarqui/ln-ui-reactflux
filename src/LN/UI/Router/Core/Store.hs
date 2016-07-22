{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.Router.Core.Store (
  CoreStore (..),
  defaultCoreStore,
  CoreAction (..),
  coreStore,
  coreView,
  coreView_
) where



import           Control.DeepSeq           (NFData)
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Monoid               ((<>))
import           Data.Rehtie               (rehtie)
import           Data.Text                 (Text)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           React.Flux                hiding (view)
import qualified React.Flux                as RF

import           LN.Api                    (getMe')
import           LN.T.User                 (UserResponse (..))
import qualified LN.UI.App.About           as App
import qualified LN.UI.App.Home            as App
import qualified LN.UI.App.Organization    as App
import qualified LN.UI.App.Organizations   as App
import qualified LN.UI.App.Portal          as App
import           LN.UI.HaskellApiHelpers   (rd)
import           LN.UI.ReactFlux.DOM       (ahref, ahrefName)
import           LN.UI.Router.Class.App
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Routes
import           LN.UI.State.PageInfo      (PageInfo, defaultPageInfo)



data CoreStore = CoreStore {
  coreStore_Route    :: RouteWith,
  coreStore_Me       :: Maybe UserResponse,
  coreStore_PageInfo :: PageInfo
} deriving (Typeable, Generic)

defaultCoreStore :: CoreStore
defaultCoreStore = CoreStore {
  coreStore_Route    = routeWith' Home,
  coreStore_Me       = Nothing,
  coreStore_PageInfo = defaultPageInfo
}



data CoreAction
  = Core_Init
  | Core_Route RouteWith
  | Core_Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData CoreStore where
  type StoreAction CoreStore = CoreAction
  transform action st@CoreStore{..} = do
    case action of
      Core_Init        -> action_core_init
      Core_Route route -> action_core_route route
      _                -> pure st
    where
    action_core_init = do
      putStrLn "Core_Init"
      lr <- rd getMe'
      rehtie lr (const $ pure st) $ \user_pack ->
        pure $ st{ coreStore_Me = Just user_pack }

    action_core_route route = do

      putStrLn $ show route

      case route of

        RouteWith Home _                       -> pure ()
        RouteWith About _                      -> pure ()
        RouteWith Portal _                     -> pure ()
        RouteWith (Organizations Index) params -> pure ()
        RouteWith (Users Index) params         -> pure ()
        RouteWith _ _                          -> pure ()

      pure $ st{ coreStore_Route = route }



coreStore :: ReactStore CoreStore
coreStore = mkStore defaultCoreStore



coreView :: ReactView CoreStore
coreView =
  defineControllerView "core" coreStore $ \st _ ->
    defaultLayout st (renderRouteView st)



coreView_ :: CoreStore  -> ReactElementM eventHandler ()
coreView_ st =
  RF.view coreView st mempty



defaultLayout :: CoreStore -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
defaultLayout st@CoreStore{..} page =
  div_ $ do
    navBar coreStore_Me
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



renderRouteView :: CoreStore -> ReactElementM ViewEventHandler ()
renderRouteView CoreStore{..} = do
  div_ $ do
    case coreStore_Route of
      RouteWith Home _                        -> App.homeView_
      RouteWith About _                       -> App.aboutView_
      RouteWith Portal _                      -> App.portalView_
      RouteWith (Organizations Index) params  -> App.organizationsView_
      RouteWith (Organizations crud) params   -> App.organizationView_ crud
      RouteWith (Users Index) params          -> p_ $ elemText "Users Index"
      RouteWith (Users crud) params           -> p_ $ elemText "Users crud"
      RouteWith _ _                           -> p_ $ elemText "Unknown"
