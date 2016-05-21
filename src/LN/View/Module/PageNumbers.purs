module LN.View.Module.PageNumbers (
  renderPageNumbers,
  pageRange
) where



import Data.Array                      (range)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (show, map, ($), (+), (-), (<), (>), (==), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes')
import LN.Router.Types                 (Routes)
import LN.State.PageInfo               (PageInfo)
import LN.T



pageRange :: PageInfo -> Array Int
pageRange pageInfo = range 1 pageInfo.totalPages



renderPageNumbers :: PageInfo -> Routes -> ComponentHTML Input
renderPageNumbers pageInfo route =
  H.div [P.class_ B.centerBlock] [
    H.ul [P.class_ (H.className "pagination")]
    $
      map (\a ->
        a
      ) $ renderPageNumbers' pageInfo route
  ]



renderPageNumbers' :: PageInfo -> Routes -> Array (HTML _ _)
renderPageNumbers' pageInfo route =

  [H.li [] [linkToP_Classes' [Limit pageInfo.resultsPerPage, Offset prev] route "prev"]]
  <>
  (map (\page ->
      H.li (classes page) [linkToP_Classes' [Limit pageInfo.resultsPerPage, Offset page] route (show page)]
  ) $ pageRange pageInfo)
  <>
  [H.li [] [linkToP_Classes' [Limit pageInfo.resultsPerPage, Offset next] route "next"]]
  where
  prev = let p = (pageInfo.currentPage - 1) in if p < 1 then 1 else p
  next = let p = (pageInfo.currentPage + 1) in if (p > pageInfo.totalPages) then pageInfo.totalPages else p
  classes p =
    if p == pageInfo.currentPage
      then [P.classes [B.active]]
      else []
