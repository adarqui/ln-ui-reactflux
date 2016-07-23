{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Breadcrumbs (
  view_,
  view
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Route



view_ :: RouteWith -> ReactElementM eventHandler ()
view_ route_with =
  RF.view view route_with mempty



view :: ReactView RouteWith
view = defineView "breadcrumbs" $ \(RouteWith route params) ->
  case (crumb route) of
    [] -> pure ()
    xs -> do
      div_ $ p_ $ do
        ol_ $
          mapM_ (\breadcrumb -> li_ $ ahref $ routeWith' breadcrumb) xs
