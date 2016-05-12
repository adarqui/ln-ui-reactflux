module LN.State.PageInfo (
  PageInfo,
  defaultPageInfo,
  defaultPageInfo_Users,
  defaultPageInfo_Organizations,
  defaultPageInfo_Forums,
  defaultPageInfo_Threads,
  defaultPageInfo_ThreadPosts
) where



import LN.T.Internal.Types (SortOrderBy(..), OrderBy(..))



type PageInfo =
  { currentPage :: Int
  , resultsPerPage :: Int
  , totalResults :: Int
  , totalPages :: Int
  , sortOrder :: SortOrderBy
  , order :: OrderBy
  }



defaultPageInfo :: PageInfo
defaultPageInfo = { currentPage: 1, resultsPerPage: 20, totalResults: 0, totalPages: 1, sortOrder: SortOrderBy_Asc, order: OrderBy_Id }



defaultPageInfo_Users :: PageInfo
defaultPageInfo_Users = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



defaultPageInfo_Organizations :: PageInfo
defaultPageInfo_Organizations = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



defaultPageInfo_Forums :: PageInfo
defaultPageInfo_Forums = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



defaultPageInfo_Boards :: PageInfo
defaultPageInfo_Boards = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }



defaultPageInfo_Threads :: PageInfo
defaultPageInfo_Threads = defaultPageInfo { sortOrder = SortOrderBy_Dsc, order = OrderBy_ActivityAt }



defaultPageInfo_ThreadPosts :: PageInfo
defaultPageInfo_ThreadPosts = defaultPageInfo -- { orderBy = OrderBy_CreatedAt }
