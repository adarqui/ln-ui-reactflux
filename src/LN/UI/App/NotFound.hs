{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.NotFound (
  view_,
  view
) where



import           React.Flux  hiding (view)
import qualified React.Flux  as RF

import           LN.UI.Types (HTMLEvent_)



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineView "not-found" $ \_ ->
  notFoundImg



notFoundImg :: HTMLEvent_
notFoundImg = img_ ["src" $= "/static/img/404.png", "alt" $= "404 page not found"] mempty
