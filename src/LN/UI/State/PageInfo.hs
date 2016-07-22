{-# LANGUAGE BangPatterns #-}

module LN.UI.State.PageInfo (
  PageInfo (..),
  defaultPageInfo,
  pageInfoFromParams,
  -- defaultPageInfo,
  -- defaultPageInfo_Users,
  -- defaultPageInfo_Organizations,
  -- defaultPageInfo_Forums,
  -- defaultPageInfo_Threads,
  -- defaultPageInfo_ThreadPosts,
  -- defaultPageInfo_Resources,
  -- defaultPageInfo_Leurons,
  -- defaultPageInfo_Workouts,
  -- RunPageInfo,
  -- runPageInfo
) where



import           Data.List  (head)
import           Data.Maybe (maybe)
import qualified Data.Map as Map (lookup)

import           LN.T       (ParamTag(..), SortOrderBy(..), OrderBy(..), CountResponses, OrderBy (..), Param (..),
                             SortOrderBy (..))
import LN.UI.Router.Class.Params (Params)



data PageInfo = PageInfo {
  currentPage    :: !Int,
  resultsPerPage :: !Int,
  totalResults   :: !Int,
  totalPages     :: !Int,
  sortOrder      :: !SortOrderBy,
  order          :: !OrderBy
} deriving (Show)



defaultPageInfo :: PageInfo
defaultPageInfo = PageInfo {
  currentPage    = defaultCurrentPage,
  resultsPerPage = defaultResultsPerPage,
  totalResults   = defaultTotalResults,
  totalPages     = defaultTotalPages,
  sortOrder      = SortOrderBy_Asc,
  order          = OrderBy_Id
}



defaultCurrentPage :: Int
defaultCurrentPage = 1

defaultResultsPerPage :: Int
defaultResultsPerPage = 20

defaultTotalResults :: Int
defaultTotalResults = 0

defaultTotalPages :: Int
defaultTotalPages = 1

defaultSortOrder :: SortOrderBy
defaultSortOrder = SortOrderBy_Asc

defaultOrder :: OrderBy
defaultOrder = OrderBy_Id



pageInfoFromParams :: Params -> PageInfo
pageInfoFromParams params =
  PageInfo {
    currentPage    = maybe defaultCurrentPage (\(Offset offset) -> offset) m_offset,
    resultsPerPage = maybe defaultResultsPerPage (\(Limit limit) -> limit) m_limit,
    totalResults   = 0,
    totalPages     = 1,
    sortOrder      = maybe defaultSortOrder (\(SortOrder sort_order) -> sort_order) m_sort_order,
    order          = maybe defaultOrder (\(Order order) -> order) m_order
  }
  where
  m_offset     = Map.lookup ParamTag_Offset params
  m_limit      = Map.lookup ParamTag_Limit params
  m_sort_order = Map.lookup ParamTag_SortOrder params
  m_order      = Map.lookup ParamTag_Order params



-- defaultPageInfo_Users :: PageInfo
-- defaultPageInfo_Users = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Organizations :: PageInfo
-- defaultPageInfo_Organizations = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Forums :: PageInfo
-- defaultPageInfo_Forums = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Boards :: PageInfo
-- defaultPageInfo_Boards = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Threads :: PageInfo
-- defaultPageInfo_Threads = defaultPageInfo { sortOrder = SortOrderBy_Dsc, order = OrderBy_ActivityAt }



-- defaultPageInfo_ThreadPosts :: PageInfo
-- defaultPageInfo_ThreadPosts = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Resources :: PageInfo
-- defaultPageInfo_Resources = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Leurons :: PageInfo
-- defaultPageInfo_Leurons = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- defaultPageInfo_Workouts :: PageInfo
-- defaultPageInfo_Workouts = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



-- -- For page numbers stuff, inside Eval components


-- type RunPageInfo = {
--   count    :: Int,
--   pageInfo :: PageInfo,
--   params   :: Array Param
-- }



-- runPageInfo :: CountResponses -> PageInfo -> RunPageInfo
-- runPageInfo count_responses page_info =
--   {
--     count: count,
--     pageInfo: pi,
--     params: par
--   }
--   where
--   count =
--     maybe
--       0
--       (\count_response -> count_response ^. _CountResponse .. n_)
--       (head (count_responses ^. _CountResponses .. countResponses_))
--   pi  =
--     page_info {
--       totalResults = count,
--       totalPages   = (count / page_info.resultsPerPage) + 1
--     }
--   par =
--     [ Limit page_info.resultsPerPage
--     , Offset ((page_info.currentPage - 1) * page_info.resultsPerPage)
--     , SortOrder page_info.sortOrder
--     , Order page_info.order
--     ]
