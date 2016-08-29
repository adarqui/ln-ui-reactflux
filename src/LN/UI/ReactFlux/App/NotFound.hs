{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.NotFound (
  view
) where



import           React.Flux                            hiding (view)

import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types                 (HTMLView_)



view :: HTMLView_
view = defineViewWithSKey "not-found" () $ \_ ->
  img_ ["src" $= "/static/img/404.png", "alt" $= "404 page not found"] mempty
