{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Breadcrumbs (
  view_,
  view
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router
import           LN.UI.Types                (HTMLEvent_)



view_ :: RouteWith -> HTMLEvent_
view_ route_with =
  RF.view view route_with mempty



view :: ReactView RouteWith
view = defineView "breadcrumbs" $ \(RouteWith route params) ->
  case (crumb route) of
    [] -> mempty
    xs -> do
      div_ $ p_ $ do
        ol_ [ "className" $= "breadcrumb" ] $
          mapM_ (\breadcrumb -> li_ $ ahref $ routeWith' breadcrumb) xs
