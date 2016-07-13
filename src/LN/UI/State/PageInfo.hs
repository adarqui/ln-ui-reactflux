module LN.UI.State.PageInfo (
  PageInfo (..),
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

import           LN.T       (CountResponses, OrderBy (..), Param (..),
                             SortOrderBy (..))



data PageInfo = PageInfo {
  currentPage    :: Int,
  resultsPerPage :: Int,
  totalResults   :: Int,
  totalPages     :: Int,
  sortOrder      :: SortOrderBy,
  order          :: OrderBy
  }



defaultPageInfo :: PageInfo
defaultPageInfo = PageInfo {
  currentPage = 1,
  resultsPerPage = 20,
  totalResults = 0,
  totalPages = 1,
  sortOrder = SortOrderBy_Asc,
  order = OrderBy_Id
  }



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
