{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.About (
  view,
  view_
) where



import           React.Flux      hiding (view)
import qualified React.Flux      as RF



view :: ReactView ()
view = defineView "about" $ \_ ->
  div_ $ p_ $ elemText "LN is a full stack haskell system which powers adarq.org"



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty
