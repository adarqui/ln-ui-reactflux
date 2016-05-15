module LN.Eval.Goto (
  eval_Goto
) where



import Daimyo.Data.Array      (elemBy)
import Data.Functor           (($>))
import Data.Maybe             (maybe)
import Data.Int               (fromString)
import Data.Tuple             (Tuple(..))
import Halogen                (get, gets, modify, liftAff')
import Optic.Core             ((^.),(..))
import Prelude                (show, bind, pure, unit, id, (==), (/=), (<))

import LN.Component.Types     (EvalEff)
import LN.Input.Types         (Input(..))
import LN.Router.Internal     (updateUrl)
import LN.Router.Types        (Routes(..), CRUD(..))
import LN.T



eval_Goto :: EvalEff
eval_Goto eval (Goto route next) = do

  modify (_ { currentPage = route })
  liftAff' (updateUrl route)

  st <- get

  case route of


    Me          -> do

      eval (GetMe next)

{-
      st' <- get

      case (st' ^. stMe) of
           Nothing    -> pure unit
           Just user  -> liftAff' $ updateUrl (Users (Show (user ^. _UserPackResponse .. user_ ^. _UserResponse .. name_)))
           -}

      pure unit


    PortalOrganizations -> eval (GetOrganizations next) $> unit


    (PortalUsers params) -> do
      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
      maybe
        (pure unit)
        (\(Tuple k offset) -> do
          pageInfo <- gets _.usersPageInfo
          modify (_{ usersPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
          pure unit)
        moffset
      eval (GetUsers next) $> unit



    (Organizations (Show org_name)) -> eval (GetOrganization org_name next) $> unit



    (OrganizationsForums org_name (Show forum_name) params) -> do
      -- TODO FIXME
      if (org_name /= (maybe "" (\v -> v ^. _OrganizationResponse .. name_) st.currentOrganization))
        then eval (GetOrganization org_name next)
        else pure next

      eval (GetOrganizationForum org_name forum_name next) $> unit



    (OrganizationsForumsBoards org_name forum_name (Show board_name) params) -> do
      -- TODO FIXME
      if (org_name /= (maybe "" (\v -> v ^. _OrganizationResponse .. name_) st.currentOrganization))
        then eval (GetOrganization org_name next)
        else pure next

      if (forum_name /= (maybe "" (\v -> v ^. _ForumResponse .. name_) st.currentForum))
        then eval (GetOrganizationForum org_name forum_name next)
        else pure next

      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
      maybe
        (pure unit)
        (\(Tuple k offset) -> do
          pageInfo <- gets _.threadsPageInfo
          modify (_{ threadsPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
          pure unit)
        moffset

      let morder = elemBy (\(Tuple k v) -> k == show ParamTag_Order) params
      maybe
        (pure unit)
        (\(Tuple k order_by) -> do
          pageInfo <- gets _.threadsPageInfo
          modify (_{ threadsPageInfo = pageInfo { order = orderFromString order_by } })
          pure unit)
        morder

      let msort_order = elemBy (\(Tuple k v) -> k == show ParamTag_SortOrder) params
      maybe
        (pure unit)
        (\(Tuple k order) -> do
          pageInfo <- gets _.threadsPageInfo
          modify (_{ threadsPageInfo = pageInfo { sortOrder = sortOrderFromString order } })
          pure unit)
        msort_order

      eval (GetOrganizationForumBoard org_name forum_name board_name next) $> unit



    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread_name) params) -> do
      -- TODO FIXME
      if (org_name /= (maybe "" (\v -> v ^. _OrganizationResponse .. name_) st.currentOrganization))
         then eval (GetOrganization org_name next)
         else pure next

      if (forum_name /= (maybe "" (\v -> v ^. _ForumResponse .. name_) st.currentForum))
         then eval (GetOrganizationForum org_name forum_name next)
         else pure next

      if (board_name /= (maybe "" (\v -> v ^. _BoardResponse .. name_) st.currentBoard))
         then eval (GetOrganizationForumBoard org_name forum_name board_name next)
         else pure next

      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
      maybe
        (pure unit)
        (\(Tuple k offset) -> do
          pageInfo <- gets _.threadPostsPageInfo
          modify (_{
            threadPostsPageInfo = pageInfo
            -- TODO FIXME: offset=-1 just makes us go to the last page
              { currentPage = let off = maybe 1 id (fromString offset) in if off < 0 then pageInfo.totalPages else off }
          })
          pure unit)
        moffset

      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) $> unit


    (Resources Index params) -> do
      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
      maybe
        (pure unit)
        (\(Tuple k offset) -> do
          pageInfo <- gets _.resourcesPageInfo
          modify (_{ resourcesPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
          pure unit)
        moffset
      eval (GetResources next) $> unit

    (Leurons params) -> eval (GetLeurons next) $> unit


    (Users (Show user_nick)) -> do

      eval (GetUser user_nick next) $> unit

    (UsersProfile user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersSettings user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersPMs user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersThreads user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersThreadPosts user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersWorkouts user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersResources user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersLeurons user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersLikes user_nick params) -> eval (GetUser user_nick next) $> unit



{-
    (ThreadPosts (Show thread_post_id)) -> eval (GetThreadPost thread_post_id next) $> unit
    -}

    _           -> pure unit

--  liftAff' (updateUrl route)
  pure next
