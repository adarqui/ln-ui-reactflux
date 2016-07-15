{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.Home (
  HomeState,
  HomeAction (..),
  store,
  view,
  view_
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF



data HomeState = HomeState
  deriving (Show, Typeable, Generic, NFData)



data HomeAction = HomeAction
  deriving (Show, Typeable, Generic, NFData)



instance StoreData HomeState where
  type StoreAction HomeState = HomeAction
  transform action st = do
    putStrLn "Home"
    pure HomeState



store :: ReactStore HomeState
store = mkStore HomeState



view :: ReactView HomeState
view = defineView "home" $ \st ->
  div_ $ p_ $ elemText "Home"



view_ :: HomeState -> ReactElementM eventHandler ()
view_ st =
  RF.view view st mempty
