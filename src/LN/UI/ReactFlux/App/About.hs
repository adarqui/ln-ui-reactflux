{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.About (
  view_,
  view
) where



import           React.Flux  hiding (view)
import qualified React.Flux  as RF

import           LN.UI.ReactFlux.Types (HTMLEvent_)



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineView "about" $ \_ ->
  div_ $ p_ $ elemText "LN is a full stack haskell system which powers adarq.org"
