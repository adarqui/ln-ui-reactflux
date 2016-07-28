{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | The sole purpose of this module is to update the hash based on a Goto Action
--

module LN.UI.ReactFlux.App.Route (
  Store,
  defaultStore,
  Action (..),
  store,
  dispatch
) where



import           Control.DeepSeq     (NFData)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)
import           GHCJS.Router.Base   (setLocationHash)
import           React.Flux          hiding (view)
import qualified React.Flux          as RF
import qualified Data.Text as Text (unpack)

import           LN.UI.Core.Helpers.GHCJS
import           LN.UI.Core.Router.Route  (RouteWith (..), fromRouteWith)



data Store = Store
  deriving (Show, Typeable, Generic, NFData)



data Action
  = Goto RouteWith
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = case action of
    Goto route_with -> do
      setLocationHash $ Text.unpack $ fromRouteWith route_with
      pure st



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
