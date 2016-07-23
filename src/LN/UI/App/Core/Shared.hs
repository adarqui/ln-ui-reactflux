{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Core.Shared (
  Store (..),
  Action (..),
  defaultStore
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
  _route :: RouteWith,
  _me    :: Maybe UserResponse
} deriving (Typeable, Generic)



data Action
  = Init
  | SetRoute RouteWith
  | Nop
  deriving (Show, Typeable, Generic, NFData)



defaultStore :: Store
defaultStore = Store {
  _route    = routeWith' Home,
  _me       = Nothing
}
