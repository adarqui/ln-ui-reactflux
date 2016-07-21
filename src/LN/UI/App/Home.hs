{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.Home (
  HomeStore,
  HomeAction (..),
  homeStore,
  homeView,
  homeView_
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF



data HomeStore = HomeStore
  deriving (Show, Typeable, Generic, NFData)



data HomeAction = HomeAction
  deriving (Show, Typeable, Generic, NFData)



instance StoreData HomeStore where
  type StoreAction HomeStore = HomeAction
  transform action st = do
    putStrLn "Home"
    pure HomeStore



homeStore :: ReactStore HomeStore
homeStore = mkStore HomeStore



homeView :: ReactView HomeStore
homeView = defineView "home" $ \st ->
  div_ $ p_ $ elemText "Welcome to LN!"



homeView_ :: HomeStore -> ReactElementM eventHandler ()
homeView_ st =
  RF.view homeView st mempty
