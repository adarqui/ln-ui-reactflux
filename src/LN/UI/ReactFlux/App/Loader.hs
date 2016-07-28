{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

module LN.UI.ReactFlux.App.Loader (
   module A
 , loadingImg
 , unexpectedError
) where



import           React.Flux            hiding (view)

import           LN.UI.Core.Loader as A
import           LN.UI.ReactFlux.Types (HTMLView_)



instance HasLoader (ReactElementM ViewEventHandler ()) where
  loading  = loadingImg
  cantLoad = unexpectedError



loadingImg :: HTMLView_
loadingImg = img_ ["src" $= "/static/img/loading/2.gif", "alt" $= "loading"] mempty



unexpectedError :: HTMLView_
unexpectedError = div_ $ p_ $ elemText "Unexpected error loading resource."
