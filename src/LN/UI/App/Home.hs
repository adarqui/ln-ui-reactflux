{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.Home (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF



data Store = Store
  deriving (Show, Typeable, Generic, NFData)



data Action = Action
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Home"
    pure Store



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineControllerView "home" store $ \st _ ->
  div_ $ p_ $ elemText "Welcome to LN!"
