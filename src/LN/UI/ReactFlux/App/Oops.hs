{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.Oops (
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
view = defineView "oops" $ \_ ->
  oopsImg



oopsImg :: HTMLEvent_
oopsImg = img_ ["src" $= "/static/img/oops.png", "alt" $= "Oops... something went wrong."] mempty
