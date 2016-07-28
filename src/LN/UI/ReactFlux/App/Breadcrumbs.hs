{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Breadcrumbs (
  view_,
  view
) where



import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.UI.Core.Router
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, className_)
import           LN.UI.ReactFlux.Types                (HTMLEvent_)



view_ :: RouteWith -> HTMLEvent_
view_ route_with =
  RF.view view route_with mempty



view :: ReactView RouteWith
view = defineView "breadcrumbs" $ \(RouteWith route params) ->
  case (crumb route) of
    [] -> mempty
    xs -> do
      div_ $ p_ $ do
        ol_ [className_ B.breadcrumb] $
          mapM_ (\breadcrumb -> li_ $ ahref $ routeWith' breadcrumb) xs
