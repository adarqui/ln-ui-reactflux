{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.PageNumbers (
  pageNumbersView,
  pageNumbersView_,
  pageRange
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.DataText     (tshow)
import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Route
import           LN.UI.State.PageInfo       (PageInfo (..))



type Pages =
  (Maybe Int  -- prev
  ,[Int]      -- pages
  ,Maybe Int  -- next
  ,Int        -- limit
  )



pageNumbersView :: ReactView (PageInfo, RouteWith)
pageNumbersView = defineView "pageNumbers" $ \(page_info, route_with@(RouteWith route params)) -> do
  let (m_prev, pages, m_next, limit) = buildPages page_info route_with
  div_ $ do
    ul_ $ do
      li_ $ ahrefName "prev" route_with
      mapM_ (\page_number -> li_ $ ahrefName (tshow page_number) route_with) pages
      li_ $ ahrefName "next" route_with



pageNumbersView_ :: (PageInfo, RouteWith) -> ReactElementM eventHandler ()
pageNumbersView_ (page_info, route_with) =
  RF.view pageNumbersView (page_info, route_with) mempty



pageRange :: PageInfo -> [Int]
pageRange PageInfo{..} = [1..totalPages]



buildPages :: PageInfo -> RouteWith -> Pages
buildPages PageInfo{..} (RouteWith route params) =
  ( prev
  , []
  , next
  , resultsPerPage
  )
  where
  prev = let p = (currentPage - 1) in if p < 1 then Nothing else (Just p)
  next = let p = (currentPage + 1) in if (p > totalPages) then Nothing else (Just p)
