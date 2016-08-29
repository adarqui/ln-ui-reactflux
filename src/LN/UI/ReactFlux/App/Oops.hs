{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.Oops (
  view
) where



import           React.Flux                            hiding (view)

import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types                 (HTMLView_)



view :: HTMLView_
view = defineViewWithSKey "oops" () $ \_ ->
  img_ ["src" $= "/static/img/oops.png", "alt" $= "Oops... something went wrong."] mempty
