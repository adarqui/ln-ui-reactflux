{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.Router.Core.Store (
  CoreStore (..),
  defaultCoreStore,
  CoreAction (..),
  coreStore,
  coreView,
  coreView_
) where



import           Control.DeepSeq        (NFData)
import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import           LN.UI.Router.Class.App
import           React.Flux             hiding (view)
import qualified React.Flux             as RF



data CoreStore = CoreStore {
  coreStoreRoute :: RoutingApp
} deriving (Show, Typeable, Generic, NFData)

defaultCoreStore :: CoreStore
defaultCoreStore = CoreStore {
  coreStoreRoute = AppHome
}



data CoreAction
  = Core_SetHash Text
  | Core_Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData CoreStore where
  type StoreAction CoreStore = CoreAction
  transform action st = do
    putStrLn "Core"
    pure st



coreStore :: ReactStore CoreStore
coreStore = mkStore defaultCoreStore



coreView :: ReactView CoreStore
coreView =
  defineView "core" $ \st ->
  div_ $ p_ $ elemText "Core"



coreView_ :: CoreStore -> ReactElementM eventHandler ()
coreView_ st =
  RF.view coreView st mempty
