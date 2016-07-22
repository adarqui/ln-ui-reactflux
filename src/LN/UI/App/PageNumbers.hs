{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.PageNumbers (
  pageNumbersView,
  pageNumbersView_
) where



import           React.Flux                hiding (view)
import qualified React.Flux                as RF

import           LN.UI.ReactFlux.DOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Route
import LN.UI.State.PageInfo (PageInfo(..))



pageNumbersView :: ReactView (PageInfo, RouteWith)
pageNumbersView = defineView "pageNumbers" $ \(page_numbers, RouteWith route params) ->
  case (crumb route) of
    [] -> pure ()
    xs -> do
      div_ $ p_ $ do
        ol_ $
          mapM_ (\breadcrumb -> li_ $ ahref $ routeWith' breadcrumb) xs



pageNumbersView_ :: (PageInfo, RouteWith) -> ReactElementM eventHandler ()
pageNumbersView_ (page_info, route_with) =
  RF.view pageNumbersView (page_info, route_with) mempty
