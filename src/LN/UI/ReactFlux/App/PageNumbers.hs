{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.PageNumbers (
  view_,
  view,
  pageRange,
  buildPages,
  runPageInfo
) where



import           Control.Monad                        (forM_)
import           Data.Int                             (Int64)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.T.Count                           (CountResponse (..),
                                                       CountResponses (..))
import           LN.UI.Core.Helpers.DataText          (tshow)
import           LN.UI.Core.PageInfo                  (PageInfo (..))
import           LN.UI.Core.Router                    (RouteWith (..), updateParams_Offset_Limit)
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahrefName, classNames_)
import           LN.UI.ReactFlux.Types                (HTMLEvent_)



type Pages =
  (Int64   -- prev
  ,[Int64] -- pages
  ,Int64   -- next
  ,Int64   -- limit
  )



view_ :: (PageInfo, RouteWith) -> HTMLEvent_
view_ (page_info, route_with) =
  RF.view view (page_info, route_with) mempty



view :: ReactView (PageInfo, RouteWith)
view = defineView "pageNumbers" $ \(page_info, route_with@(RouteWith route params)) -> do
  let
    (prev, pages, next, limit) = buildPages page_info route_with
    upd off                    = RouteWith route (updateParams_Offset_Limit (off*limit) limit params)
  case pages of
    []     -> mempty
    _      ->
      div_ $ do
        ul_ [classNames_ [B.pagination, B.paginationSm]] $ do
          li_ ["key" $= "pg-prev"] $ ahrefName "prev" (upd prev)
          forM_ (zip [(1::Int)..] pages) $ \(idx, page_number) -> li_ ["key" @= idx] $ ahrefName (tshow page_number) (upd page_number)
          li_ ["key" $= "pg-next"] $ ahrefName "next" (upd next)



-- | If only one page exists, we consider it empty.
--
pageRange :: PageInfo -> [Int64]
pageRange PageInfo{..} =
  case [1..totalPages] of
    [_] -> []
    xs  -> xs



buildPages :: PageInfo -> RouteWith -> Pages
buildPages page_info@PageInfo{..} _ =
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
        totalPages   = (countResponseN `div` resultsPerPage page_info)
      }
    _      -> page_info
