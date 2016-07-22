{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.Organization (
  Store,
  defaultStore,
  Action (..),
  store,
  view,
  view_
) where



import           Control.DeepSeq         (NFData)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           LN.UI.Router.Class.CRUD
import           React.Flux              hiding (view)
import qualified React.Flux              as RF



data Store = Store
  deriving (Show, Typeable, Generic, NFData)



data Action = Action
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Organization"
    pure Store



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store



view :: ReactView CRUD
view = defineControllerView "organization" store $ \st _ ->
  div_ $ p_ $ elemText "Welcome to LN!"



view_ :: CRUD -> ReactElementM eventHandler ()
view_ crud =
  RF.view view crud mempty
