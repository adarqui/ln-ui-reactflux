{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, liKey_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types                 (HTMLView_)



view :: HTMLView_
view = defineViewWithSKey "portal" () $ \_ ->
  div_ $ do
    viewHeader
    viewLinks



viewHeader :: HTMLView_
viewHeader =
  defineViewWithSKey "portal-header" () go
  where
  go :: () -> HTMLView_
  go _ = h1_ "Portal"



viewLinks :: HTMLView_
viewLinks =
  defineViewWithSKey "portal-links" () go
  where
  go :: () -> HTMLView_
  go _ = do
    ol_ $ do
      liKey_ "portal-link-organizations" $ ahref $ routeWith' (Organizations Index)
      liKey_ "portal-link-users"         $ ahref $ routeWith' (Users Index)
