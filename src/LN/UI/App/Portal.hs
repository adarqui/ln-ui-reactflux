{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Portal (
  view,
  view_
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Route



view :: ReactView ()
view = defineView "portal" $ \_ ->
  div_ $ p_ $ do
    h1_ "Portal"
    ol_ $ do
      li_ $ ahref $ routeWith' (Organizations Index)
      li_ $ ahref $ routeWith' (Users Index)



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty
