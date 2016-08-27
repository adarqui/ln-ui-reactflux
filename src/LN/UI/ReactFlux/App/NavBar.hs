{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans  #-}

module LN.UI.ReactFlux.App.NavBar (
  view
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (void)
import           Control.Monad.IO.Class                (liftIO)
import           Data.List                             ((\\))
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import           React.Flux.Router.WebRoutes           (initRouterRaw'ByteString)
import qualified Web.Bootstrap3                        as B

import           LN.T.User                             (UserResponse (..))
import           LN.UI.Core.Control                    (CoreResult (..))
import qualified LN.UI.Core.Loader                     as Loader (loader1)
import           LN.UI.Core.PageInfo                   (PageInfo,
                                                        defaultPageInfo)
import           LN.UI.Core.Router
import           LN.UI.Core.State                      (Action (..), Store (..),
                                                        defaultStore)
import           LN.UI.ReactFlux.App.Core.Shared
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types



view :: Maybe UserResponse -> RouteWith -> HTMLView_
view !m_user' !route_with' = do
  defineViewWithSKey "navbar" (m_user', route_with') go
  where
  go :: (Maybe UserResponse, RouteWith) -> HTMLView_
  go (m_user, route_with) = do
    cldiv_ B.containerFluid $ do
      nav_ [classNames_ [B.navbarNav, B.navbarStaticTop]] $ do
        cldiv_ B.container $ do

          ahrefClassesKey "nav-home" [B.navbarBrand] $ routeWith' Home
          ul_ ["key" $= "nav-ul", classNames_ [B.navbarNav, B.nav, B.navTabs]] $ do
            li_ ["key" $= "nav-about"]  $ ahrefKey "nav-about" $ routeWith' About
            li_ ["key" $= "nav-portal"] $ ahrefKey "nav-portal" $ routeWith' Portal

            case m_user of
              Nothing               -> mempty
              Just UserResponse{..} -> do
                li_ ["key" $= "nav-me"] $ ahrefNameKey "nav-me" "Me" $ routeWith' $ Users (ShowS userResponseName)

            li_ ["key" $= "nav-user"]   $ do
              case m_user of
                Nothing               -> ahrefKey "nav-user" $ routeWith' Login
                                         -- Raw anchor here, to hit the server's /auth/logout
                Just UserResponse{..} -> a_ ["key" $= "nav-user", "href" $= "/auth/logout"] $ elemText ("Logout: " <> userResponseName)
            li_ ["key" $= "nav-refresh"] $ do
              -- A method for refreshing the current route, without actually refreshing the page from the browser
              --
              button_ [ classNames_ [B.btn, B.btnDefault, B.btnXs]
                      , onClick $ \_ _ -> dispatch $ Route route_with
                      ] $ span_ [classNames_ [B.glyphicon, B.glyphiconRefresh]] mempty
