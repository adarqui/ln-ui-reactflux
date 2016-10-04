{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.PageNumbers (
    view
  , view_
  , view1
  , view2
  , viewAbbreviated
  , viewAbbreviated_
  , runPageInfo
) where



import           Control.Monad                         (forM_)
import           Data.Int                              (Int64)
import           React.Flux                            hiding (view)
import qualified Web.Bootstrap3                        as B

import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.GHCJS              (JSString)
import           LN.UI.Core.PageInfo                   (PageInfo (..),
                                                        runPageInfo)
import           LN.UI.Core.PageNumbers                (buildPages)
import           LN.UI.Core.Router                     (RouteWith (..),
                                                        emptyParams, updateParams_Offset_Limit)
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahrefName, classNames_, className_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types                 (HTMLView_)



view
  :: PageInfo
  -> RouteWith
  -> HTMLView_
view = view_ "page-numbers"



view1
  :: PageInfo
  -> RouteWith
  -> HTMLView_
view1 = view_ "page-numbers-1"



view2
  :: PageInfo
  -> RouteWith
  -> HTMLView_
view2 = view_ "page-numbers-2"



view_
  :: JSString
  -> PageInfo
  -> RouteWith
  -> HTMLView_

view_ !key !page_info' !route_with' =
  defineViewWithSKey key (page_info', route_with') $ \(page_info, route_with@(RouteWith route params)) -> do
    let
      (prev, pages, next, limit) = buildPages page_info route_with
      route_page page            = RouteWith route (updateParams_Offset_Limit ((page-1)*limit) limit params)
      isActive p | p == currentPage page_info = className_ "active"
                 | otherwise                  = className_ "not-active"
    case pages of
      []     -> mempty
      _      ->
        div_ $ do
          ul_ [classNames_ [B.pagination, B.paginationSm]] $ do

            li_ ["key" $= "pg-prev"] $ ahrefName "prev" $
              if prev == 1
                then RouteWith route emptyParams
                else route_page prev

            forM_ (zip [(1::Int)..] pages) $
              \(idx, page_number) ->

                li_ ["key" @= idx, isActive page_number] $ ahrefName (tshow page_number) $
                  if idx == 1
                    -- in page 1, we don't want offset/limit showing
                    then RouteWith route emptyParams
                    -- else, append offset/limit to everything
                    else route_page page_number

            li_ ["key" $= "pg-next"] $ ahrefName "next" (route_page next)




-- | Used for App/Threads.hs's index function:
-- Shows a maximum of 4 page numbers
-- 1
-- 1 2
-- 1 2 3 4
-- 1 2 .. 4 5
-- 1 2 .. 101 102
viewAbbreviated
  :: PageInfo
  -> RouteWith
  -> HTMLView_
viewAbbreviated = view_ "page-numbers-abbrv"

viewAbbreviated_
  :: JSString
  -> PageInfo
  -> RouteWith
  -> HTMLView_

viewAbbreviated_ !key page_info' !route_with' =
  defineViewWithSKey key (page_info', route_with') $ \(page_info, route_with@(RouteWith route params)) -> do
    let
      (prev, pages, next, limit) = buildPages page_info route_with
      route_page page            = RouteWith route (updateParams_Offset_Limit ((page-1)*limit) limit params)
    case pages of
      []     -> mempty
      _      ->
        div_ $ do
          ul_ [classNames_ [B.pagination, B.paginationSm]] $ do

            let
              length_pages      = fromIntegral $ length pages
              length_pages'     = length_pages `div` limit
              max_page          = if length_pages' `mod` limit > 0
                                    then length_pages'+1
                                    else length_pages'
              abbreviated_pages = if length_pages > 4
                                    then [1,2,max_page-1,max_page]
                                    else [1..length_pages']

            forM_ abbreviated_pages $
              \page_number ->

                li_ ["key" @= page_number] $ ahrefName (tshow page_number) $
                  if page_number == 1
                    -- in page 1, we don't want offset/limit showing
                    then RouteWith route emptyParams
                    -- else, append offset/limit to everything
                    else route_page page_number
