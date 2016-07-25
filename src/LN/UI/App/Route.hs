{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The sole purpose of this module is to update the hash based on a Goto Action
--

module LN.UI.App.Route (
  Store,
  defaultStore,
  Action (..),
  store,
  dispatch
) where



import           Control.DeepSeq    (NFData)
import           Data.Typeable      (Typeable)
import           GHC.Generics       (Generic)
import           LN.UI.Router.Route (RouteWith (..))
import           React.Flux         hiding (view)
import qualified React.Flux         as RF



data Store = Store
  deriving (Show, Typeable, Generic, NFData)



data Action
  = Goto RouteWith
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = case action of
    Goto route_with ->
      pure st



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
