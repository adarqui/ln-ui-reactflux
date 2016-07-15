{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module LN.UI.App.About (
  AboutState,
  AboutAction (..),
  store,
  view,
  view_
) where



import React.Flux hiding (view)
import qualified React.Flux as RF
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)



data AboutState = AboutState
  deriving (Show, Typeable, Generic, NFData)



data AboutAction = AboutAction
  deriving (Show, Typeable, Generic, NFData)



instance StoreData AboutState where
  type StoreAction AboutState = AboutAction
  transform action st = do
    putStrLn "About"
    pure AboutState



store :: ReactStore AboutState
store = mkStore AboutState



view :: ReactView AboutState
view = defineView "about" $ \st ->
  div_ $ p_ $ elemText "About"



view_ :: AboutState -> ReactElementM eventHandler ()
view_ st =
  RF.view view st mempty
