{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.About (
  AboutStore,
  AboutAction (..),
  aboutStore,
  aboutView,
  aboutView_
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF



data AboutStore = AboutStore
  deriving (Show, Typeable, Generic, NFData)



data AboutAction = AboutAction
  deriving (Show, Typeable, Generic, NFData)



instance StoreData AboutStore where
  type StoreAction AboutStore = AboutAction
  transform action st = do
    putStrLn "About"
    pure AboutStore



aboutStore :: ReactStore AboutStore
aboutStore = mkStore AboutStore



aboutView :: ReactView AboutStore
aboutView = defineView "about" $ \st ->
  div_ $ p_ $ elemText "About"



aboutView_ :: AboutStore -> ReactElementM eventHandler ()
aboutView_ st =
  RF.view aboutView st mempty
