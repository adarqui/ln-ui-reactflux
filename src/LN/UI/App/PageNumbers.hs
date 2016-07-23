{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.PageNumbers (
  view,
  view_,
  pageRange,
  buildPages,
  runPageInfo
) where



import           Data.Int                   (Int64)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.T.Count                 (CountResponse (..),
                                             CountResponses (..))
import           LN.UI.Helpers.DataText     (tshow)
import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Param   (updateParams_Offset_Limit)
import           LN.UI.Router.Class.Route
import           LN.UI.State.PageInfo       (PageInfo (..))



type Pages =
  (Int64   -- prev
  ,[Int64] -- pages
  ,Int64   -- next
  ,Int64   -- limit
  )



view :: ReactView (PageInfo, RouteWith)
view = defineView "pageNumbers" $ \(page_info, route_with@(RouteWith route params)) -> do
  let
    (prev, pages, next, limit) = buildPages page_info route_with
    upd off                    = RouteWith route (updateParams_Offset_Limit off limit params)
  case pages of
    []     -> pure ()
    _      ->
      div_ $
        ul_ $ do
          li_ $ ahrefName "prev" (upd prev)
          mapM_ (\page_number -> li_ $ ahrefName (tshow page_number) (upd page_number)) pages
          li_ $ ahrefName "next" (upd next)



view_ :: (PageInfo, RouteWith) -> ReactElementM eventHandler ()
view_ (page_info, route_with) =
  RF.view view (page_info, route_with) mempty



-- | If only one page exists, we consider it empty.
--
pageRange :: PageInfo -> [Int64]
pageRange PageInfo{..} =
  case [1..totalPages] of
    [x] -> []
    xs  -> xs



buildPages :: PageInfo -> RouteWith -> Pages
buildPages page_info@PageInfo{..} (RouteWith route params) =
  ( prev
  , pageRange page_info
  , next
  , resultsPerPage
  )
  where
  prev = let p = (currentPage - 1) in if p < 1 then 1 else p
  next = let p = (currentPage + 1) in if (p > totalPages) then totalPages else p



runPageInfo :: CountResponses -> PageInfo -> PageInfo
runPageInfo CountResponses{..} page_info =
  case countResponses of
    (CountResponse{..}:[]) ->
      page_info {
        totalResults = countResponseN,
        totalPages   = (countResponseN `div` resultsPerPage page_info) + 1
      }
    _      -> page_info
  -- {
  --   count: count,
  --   pageInfo: pi,
  --   params: par
  -- }
  where
  -- pi  =
  --   page_info {
  --     totalResults = count,
  --     totalPages   = (count / page_info.resultsPerPage) + 1
  --   }
  -- par =
  --   [ Limit page_info.resultsPerPage
  --   , Offset ((page_info.currentPage - 1) * page_info.resultsPerPage)
  --   , SortOrder page_info.sortOrder
  --   , Order page_info.order
  --   ]
