{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Portal (
  view
) where



import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF

import           LN.UI.Core.Router                     (CRUD (..), Route (..),
                                                        RouteWith (..),
                                                        routeWith')
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types                 (HTMLView_)



view :: HTMLView_
view = defineViewWithSKey "portal" () $ \_ ->
  div_ $ do
    h1_ "Portal"
    ol_ $ do
      li_ $ ahref $ routeWith' (Organizations Index)
      li_ $ ahref $ routeWith' (Users Index)
