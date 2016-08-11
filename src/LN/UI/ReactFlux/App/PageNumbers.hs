{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.PageNumbers (
  view_,
  view,
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
import           LN.UI.Core.PageInfo                  (PageInfo (..),
                                                       runPageInfo)
import           LN.UI.Core.PageNumbers               (buildPages, pageRange)
import           LN.UI.Core.Router                    (RouteWith (..), updateParams_Offset_Limit)
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahrefName, classNames_)
import           LN.UI.ReactFlux.Types                (HTMLEvent_)



view_ :: (PageInfo, RouteWith) -> HTMLEvent_
view_ (page_info, route_with) =
  RF.view view (page_info, route_with) mempty



view :: ReactView (PageInfo, RouteWith)
view = defineView "pageNumbers" $ \(page_info, route_with@(RouteWith route params)) -> do
  let
    (prev, pages, next, limit) = buildPages page_info route_with
    upd off                    = RouteWith route (updateParams_Offset_Limit ((off-1)*limit) limit params)
  case pages of
    []     -> mempty
    _      ->
      div_ $ do
        ul_ [classNames_ [B.pagination, B.paginationSm]] $ do
          li_ ["key" $= "pg-prev"] $ ahrefName "prev" (upd prev)
          forM_ (zip [(1::Int)..] pages) $ \(idx, page_number) -> li_ ["key" @= idx] $ ahrefName (tshow page_number) (upd page_number)
          li_ ["key" $= "pg-next"] $ ahrefName "next" (upd next)
