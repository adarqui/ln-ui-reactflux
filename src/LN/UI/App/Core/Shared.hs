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



import           Control.DeepSeq          (NFData)
import           Data.Int                 (Int64)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Typeable            (Typeable)
import           GHC.Generics             (Generic)

import           LN.T.Pack.Sanitized.User (UserSanitizedPackResponse (..))
import           LN.T.User                (UserResponse (..))
import           LN.UI.Router             (Route (..), RouteWith, routeWith')



data Store = Store {
  _route :: RouteWith,
  _me    :: Maybe UserResponse,
  _users :: Map Int64 UserSanitizedPackResponse
} deriving (Typeable, Generic)



data Action
  = Init
  | SetRoute RouteWith
  | SyncUsers [Int64]
  | Nop
  deriving (Show, Typeable, Generic, NFData)



defaultStore :: Store
defaultStore = Store {
  _route    = routeWith' Home,
  _me       = Nothing,
  _users    = Map.empty
}
