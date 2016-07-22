{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.About (
  aboutView,
  aboutView_
) where



import           React.Flux      hiding (view)
import qualified React.Flux      as RF



aboutView :: ReactView ()
aboutView = defineView "about" $ \_ ->
  div_ $ p_ $ elemText "LN is a full stack haskell system which powers adarq.org"



aboutView_ :: ReactElementM eventHandler ()
aboutView_ =
  RF.view aboutView () mempty
