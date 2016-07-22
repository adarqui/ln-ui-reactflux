{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Portal (
  portalView,
  portalView_
) where



import           React.Flux                hiding (view)
import qualified React.Flux                as RF

import           LN.UI.ReactFlux.DOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Routes



portalView :: ReactView ()
portalView = defineView "portal" $ \_ ->
  div_ $ p_ $ do
    h1_ "Portal"
    ol_ $ do
      li_ $ ahref $ routeWith' (Organizations Index)
      li_ $ ahref $ routeWith' (Users Index)



portalView_ :: ReactElementM eventHandler ()
portalView_ =
  RF.view portalView () mempty
