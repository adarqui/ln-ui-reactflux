{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.PageNumbers (
  view,
  view_,
  pageRange
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.DataText     (tshow)
import           LN.UI.Helpers.ReactFluxDOM
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Param   (updateParams_Offset_Limit)
import           LN.UI.Router.Class.Route
import           LN.UI.State.PageInfo       (PageInfo (..))



type Pages =
  (Int   -- prev
  ,[Int] -- pages
  ,Int   -- next
  ,Int   -- limit
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



pageRange :: PageInfo -> [Int]
pageRange PageInfo{..} = [1..totalPages]



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
