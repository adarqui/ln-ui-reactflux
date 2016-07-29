{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans  #-}

module LN.UI.ReactFlux.App.Core (
  Store (..),
  defaultStore,
  store,
  Action (..),
  view_,
  view,
  initRouter
) where



import           Control.Concurrent                   (forkIO)
import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (liftIO)
import           Data.List                            ((\\))
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Monoid                          ((<>))
import           Data.Rehtie                          (rehtie)
import           Data.Text                            (Text)
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import           React.Flux.Router.WebRoutes          (initRouterRaw'ByteString)
import qualified Web.Bootstrap3                       as B

import           LN.Api                               (getMe', getUserSanitizedPacks_ByUsersIds')
import           LN.T.Pack.Sanitized.User             (UserSanitizedPackResponse (..), UserSanitizedPackResponses (..))
import           LN.T.User                            (UserResponse (..))
import           LN.UI.Core.App                       (runCore)
import           LN.UI.Core.Control                   (CoreResult (..))
import           LN.UI.Core.Helpers.HaskellApiHelpers (rd)
import qualified LN.UI.Core.Loader                    as Loader (loader1)
import           LN.UI.Core.PageInfo                  (PageInfo,
                                                       defaultPageInfo)
import           LN.UI.Core.Router
import           LN.UI.Core.State                     (Action (..), Store (..),
                                                       defaultStore)
import qualified LN.UI.ReactFlux.App.About            as About
import qualified LN.UI.ReactFlux.App.Boards           as Boards
import qualified LN.UI.ReactFlux.App.Breadcrumbs      as Breadcrumbs
import qualified LN.UI.ReactFlux.App.Forums           as Forums
import qualified LN.UI.ReactFlux.App.Home             as Home
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound (view_)
import qualified LN.UI.ReactFlux.App.Organizations    as Organizations
import qualified LN.UI.ReactFlux.App.Portal           as Portal
import qualified LN.UI.ReactFlux.App.Users            as Users
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefClasses,
                                                       ahrefName, className_,
                                                       classNames_)
import           LN.UI.ReactFlux.Types                (HTMLEvent_, HTMLView_)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do
    case action of
      Init            -> act_init
      Route route_with -> act_route route_with
      ApplyState f     -> pure $ f st
      _                -> pure st

    where

    basedOn result_ st_ act_ = case result_ of
      ApplyFinal f -> executeAction $ SomeStoreAction store (ApplyState f)
      Apply f      -> executeAction $ SomeStoreAction store (ApplyState f)
      Refeed       -> do
        void $ forkIO $ do
          (result', st') <- runCore st_ Refeed act_
          case result' of
            Apply f      -> executeAction $ SomeStoreAction store (ApplyState f)
            ApplyFinal f -> executeAction $ SomeStoreAction store (ApplyState f)
            Refeed       -> print "basedOn: error: refeed"
            _            -> print "basedOn: error"
      _                  -> print "basedOn: error"

    act_init = do
      (result', st') <- runCore st Final Init
      void $ basedOn result' st' Init
      pure st'

    act_route route_with = do
      (result', st') <- runCore st Final (Route route_with)
      void $ basedOn result' st' (Route route_with)
      pure st'



store :: ReactStore Store
store = mkStore defaultStore



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
        liftIO $ alterStore store $ Route action



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view =
  defineControllerView "core" store $ \st _ ->
    defaultLayout st (renderRouteView st)



defaultLayout :: Store -> HTMLView_ -> HTMLView_
defaultLayout st@Store{..} page =
  div_ $ do
    Loader.loader1 _l_m_me $ \m_me -> do
      navBar m_me
      Breadcrumbs.view_ _route
      div_ page



navBar :: Maybe UserResponse -> HTMLView_
navBar m_user_pack =
  cldiv_ B.containerFluid $ do
    nav_ [classNames_ [B.navbarNav, B.navbarStaticTop]] $ do
      cldiv_ B.container $ do
        ahrefClasses [B.navbarBrand] $ routeWith' Home
        ul_ [classNames_ [B.navbarNav, B.nav, B.navTabs]] $ do
          li_ $ ahref $ routeWith' About
          li_ $ ahref $ routeWith' Portal
          li_ $ do
            case m_user_pack of
              Nothing               -> ahref $ routeWith' Login
              Just UserResponse{..} -> ahrefName ("Logout: " <> userResponseName) $ routeWith' Logout



renderRouteView :: Store -> HTMLView_
renderRouteView Store{..} = do
  div_ $ do
    case _route of
      RouteWith Home _                        -> Home.view_
      RouteWith About _                       -> About.view_
      RouteWith Portal _                      -> Portal.view_
      RouteWith (Organizations Index) _       -> Organizations.viewIndex_ _pageInfo _l_organizations
      RouteWith (Organizations crud) params   -> Organizations.view_ crud
      RouteWith (OrganizationsForums org_sid crud) params -> Forums.view_ org_sid crud
      RouteWith (OrganizationsForumsBoards org_sid forum_sid crud) params -> Boards.view_ org_sid forum_sid crud
      RouteWith (Users Index) params          -> Users.viewIndex_ _pageInfo _l_users
      RouteWith (Users crud) params           -> p_ $ elemText "Users crud"
      RouteWith _ _                           -> NotFound.view_



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
