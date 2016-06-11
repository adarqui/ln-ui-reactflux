module LN.Eval.Goto (
  eval_Goto
) where



import Daimyo.Data.Array        (elemBy)
import Data.Functor             (($>))
import Data.Maybe               (Maybe(..), maybe)
import Data.Int                 (fromString)
import Data.Tuple               (Tuple(..))
import Halogen                  (get, gets, modify, liftAff')
import Optic.Core               ((^.),(..))
import Prelude                  (show, bind, pure, unit, id, (==), (/=), (<), ($))

import LN.Component.Types       (EvalEff)
import LN.Input.Types
import LN.Input.Organization    as Organization
import LN.Input.Forum           as Forum
import LN.Internal.Organization (defaultOrganizationRequest)
import LN.Internal.Forum        (defaultForumRequest)
import LN.Internal.Board        (defaultBoardRequest)
import LN.Internal.Thread       (defaultThreadRequest)
import LN.Internal.ThreadPost   (defaultThreadPostRequest)
import LN.Internal.Leuron       (defaultLeuronRequest, leuronToTyLeuron)
import LN.Internal.Resource     (defaultResourceRequest, resourceTypeToTyResourceType)
import LN.Router.Link           (updateUrl)
import LN.Router.Types          (Routes(..), CRUD(..))
import LN.State.Organization    (defaultOrganizationRequestState)
import LN.State.Forum           (defaultForumRequestState)
import LN.State.Board           (defaultBoardRequestState)
import LN.State.Thread          (defaultThreadRequestState)
import LN.State.ThreadPost      (defaultThreadPostRequestState)
import LN.State.Leuron          (defaultLeuronRequestState, leuronRequestStateFromLeuronData)
import LN.State.Resource        (defaultResourceRequestState)
import LN.T



eval_Goto :: EvalEff
eval_Goto eval (Goto route next) = do

  modify (_ { currentPage = route })
  liftAff' (updateUrl route)

  st <- get

  case route of



    Me          -> eval (GetMe next) $> unit



    Errors      -> pure unit



    (Organizations Index params) -> eval (cOrganizationAct Organization.Gets next) $> unit

    (Organizations New params) -> do
      modify (_{ currentOrganizationRequest = Just defaultOrganizationRequest, currentOrganizationRequestSt = Just defaultOrganizationRequestState })
      pure unit

    (Organizations (Edit org_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      m_pack <- gets _.currentOrganization
      case m_pack of
           Nothing                              -> pure unit
           Just (OrganizationPackResponse pack) -> do
             m_req_st <- gets _.currentOrganizationRequestSt
             let
               org    = pack.organization ^. _OrganizationResponse
               req_st = maybe defaultOrganizationRequestState id m_req_st
             modify (_{ currentOrganizationRequest = Just $ organizationResponseToOrganizationRequest pack.organization, currentOrganizationRequestSt = Just req_st })
             pure unit

    (Organizations (Delete org_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      m_pack <- gets _.currentOrganization
      case m_pack of
           Nothing                          -> pure unit
           Just (OrganizationPackResponse pack) -> do
             modify (_{ currentOrganizationRequest = Just $ organizationResponseToOrganizationRequest pack.organization })
             pure unit

    (Organizations (Show org_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (cForumAct (Forum.Gets_ByOrganizationSid org_name) next)
      pure unit



    (OrganizationsForums org_name New params) -> do
      modify (_{ currentForumRequest = Just defaultForumRequest, currentForumRequestSt = Just defaultForumRequestState })
      pure unit

    (OrganizationsForums org_name (Edit forum_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      m_pack <- gets _.currentForum
      case m_pack of
           Nothing                              -> pure unit
           Just (ForumPackResponse pack) -> do
             m_o_st <- gets _.currentForumRequestSt
             let
               org  = pack.forum ^. _ForumResponse
               o_st = maybe defaultForumRequestState id m_o_st
             modify (_{ currentForumRequest = Just $ forumResponseToForumRequest pack.forum, currentForumRequestSt = Just o_st })
             pure unit

    (OrganizationsForums org_name (Delete forum_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      m_pack <- gets _.currentForum
      case m_pack of
           Nothing                          -> pure unit
           Just (ForumPackResponse pack) -> do
             modify (_{ currentForumRequest = Just $ forumResponseToForumRequest pack.forum })
             pure unit

    (OrganizationsForums org_name Index params) -> do
      eval (cForumAct (Forum.Gets_ByOrganizationSid org_name) next)
      pure unit

    (OrganizationsForums org_name (Show forum_name) params) -> do
      -- TODO FIXME
      if (org_name /= (maybe "" (\v -> v ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) st.currentOrganization))
        then eval (cOrganizationAct (Organization.GetSid org_name) next)
        else pure next

      eval (GetOrganizationForum org_name forum_name next) $> unit



    (OrganizationsForumsBoards org_name forum_name New params) -> do
      modify (_{ currentBoardRequest = Just defaultBoardRequest, currentBoardRequestSt = Just defaultBoardRequestState })
      pure unit

    (OrganizationsForumsBoards org_name forum_name (Edit board_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      m_pack <- gets _.currentBoard
      case m_pack of
           Nothing                              -> pure unit
           Just (BoardPackResponse pack) -> do
             m_o_st <- gets _.currentBoardRequestSt
             let
               org  = pack.board ^. _BoardResponse
               o_st = maybe defaultBoardRequestState id m_o_st
             modify (_{ currentBoardRequest = Just $ boardResponseToBoardRequest pack.board, currentBoardRequestSt = Just o_st })
             pure unit

    (OrganizationsForumsBoards org_name forum_name (Delete board_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      m_pack <- gets _.currentBoard
      case m_pack of
           Nothing                          -> pure unit
           Just (BoardPackResponse pack) -> do
             modify (_{ currentBoardRequest = Just $ boardResponseToBoardRequest pack.board })
             pure unit

    (OrganizationsForumsBoards org_name forum_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      pure unit

    (OrganizationsForumsBoards org_name forum_name (Show board_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)

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



    (OrganizationsForumsBoardsThreads org_name forum_name board_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name New params) -> do
      modify (_{ currentThreadRequest = Just defaultThreadRequest, currentThreadRequestSt = Just defaultThreadRequestState })
      pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Edit thread_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next)
      m_pack <- gets _.currentThread
      case m_pack of
           Nothing                              -> pure unit
           Just (ThreadPackResponse pack) -> do
             m_o_st <- gets _.currentThreadRequestSt
             let
               org  = pack.thread ^. _ThreadResponse
               o_st = maybe defaultThreadRequestState id m_o_st
             modify (_{ currentThreadRequest = Just $ threadResponseToThreadRequest pack.thread, currentThreadRequestSt = Just o_st })
             pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Delete thread_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next)

      m_pack <- gets _.currentThread
      case m_pack of
           Nothing                          -> pure unit
           Just (ThreadPackResponse pack) -> do
             modify (_{ currentThreadRequest = Just $ threadResponseToThreadRequest pack.thread })
             pure unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name Index params) -> do

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

--      eval (GetOrganizationForumBoard org_name forum_name board_name next) $> unit

    (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread_name) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)

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

      -- create empty thread post state
      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState })

      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) $> unit



    -- 
    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name New params) -> do
      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState })
      pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (EditI post_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next)
      eval (GetOrganizationForumBoardThreadPost org_name forum_name board_name thread_name post_id next)
      m_pack <- gets _.currentThreadPost
      case m_pack of
           Nothing                              -> pure unit
           Just (ThreadPostPackResponse pack) -> do
             m_st <- gets _.currentThreadPostRequestSt
             let
               _st = maybe defaultThreadPostRequestState id m_st
             modify (_{ currentThreadPostRequest = Just $ threadPostResponseToThreadPostRequest pack.threadPost, currentThreadPostRequestSt = Just _st })
             pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (DeleteI post_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next)
      eval (GetOrganizationForumBoardThreadPost org_name forum_name board_name thread_name post_id next)
      m_pack <- gets _.currentThreadPost
      case m_pack of
           Nothing                          -> pure unit
           Just (ThreadPostPackResponse pack) -> do
             modify (_{ currentThreadPostRequest = Just $ threadPostResponseToThreadPostRequest pack.threadPost })
             pure unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name Index params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next)

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

      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState })

      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) $> unit

    (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI post_id) params) -> do
      eval (cOrganizationAct (Organization.GetSid org_name) next)
      eval (GetOrganizationForum org_name forum_name next)
      eval (GetOrganizationForumBoard org_name forum_name board_name next)
      eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next)

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

      modify (_{ currentThreadPostRequest = Just defaultThreadPostRequest, currentThreadPostRequestSt = Just defaultThreadPostRequestState })

      eval (GetOrganizationForumBoardThreadPost org_name forum_name board_name thread_name post_id next) $> unit



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



    (Resources New params) -> do
      modify (_{ currentResourceRequest = Just defaultResourceRequest, currentResourceRequestSt = Just defaultResourceRequestState })
      pure unit

    (Resources (EditI resource_id) params)   -> do
      eval (GetResourceId resource_id next)
      m_pack <- gets _.currentResource
      case m_pack of
           Nothing                          -> pure unit
           Just (ResourcePackResponse pack) -> do
             -- TODO FIXME: St's TyResourceType needs to match source
             let
               resource = pack.resource ^. _ResourceResponse
               rst      = defaultResourceRequestState { source = resourceTypeToTyResourceType resource.source }
             modify (_{ currentResourceRequest = Just $ resourceResponseToResourceRequest pack.resource, currentResourceRequestSt = Just rst })
             pure unit

    (Resources (DeleteI resource_id) params) -> do
      eval (GetResourceId resource_id next)
      m_pack <- gets _.currentResource
      case m_pack of
           Nothing                          -> pure unit
           Just (ResourcePackResponse pack) -> do
             modify (_{ currentResourceRequest = Just $ resourceResponseToResourceRequest pack.resource })
             pure unit


    (Resources (ShowI resource_id) params)   -> eval (GetResourceId resource_id next) $> unit



    (ResourcesLeurons resource_id Index params) -> do
      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
      maybe
        (pure unit)
        (\(Tuple k offset) -> do
          pageInfo <- gets _.leuronsPageInfo
          modify (_{ leuronsPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
          pure unit)
        moffset
      eval (GetLeurons next) $> unit

    (ResourcesLeurons resource_id New params) -> do
      -- Important: don't over-write leuron request state.. we want to hold on to that info to make our lives easier
      -- when adding leurons fast
      eval (GetResourceId resource_id next) -- TODO FIXME
      lst <- gets _.currentLeuronRequestSt
      modify (_{ currentLeuronRequest = Just defaultLeuronRequest, currentLeuronRequestSt = Just $ maybe defaultLeuronRequestState id lst })
      pure unit

    (ResourcesLeurons resource_id (EditI leuron_id) params)   -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetLeuronId leuron_id next)
      m_pack <- gets _.currentLeuron
      case m_pack of
           Nothing                          -> pure unit
           Just (LeuronPackResponse pack) -> do
             -- TODO FIXME: St's TyLeuronType needs to match source
             m_lst <- gets _.currentLeuronRequestSt
             let
               leuron = pack.leuron ^. _LeuronResponse
               lst    = leuronRequestStateFromLeuronData leuron.dataP (maybe defaultLeuronRequestState id m_lst)
             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron, currentLeuronRequestSt = Just lst })
             pure unit

    (ResourcesLeurons resource_id (DeleteI leuron_id) params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetLeuronId leuron_id next)
      m_pack <- gets _.currentLeuron
      case m_pack of
           Nothing                          -> pure unit
           Just (LeuronPackResponse pack) -> do
             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron })
             pure unit

    (ResourcesLeurons resource_id (ShowI leuron_id) params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetLeuronId leuron_id next) $> unit



    (ResourcesSiftLeurons resource_id params) -> do
      pure unit

    (ResourcesSiftLeuronsLinear resource_id (ShowI offset) params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetResourceLeuronLinear resource_id offset next)
      pure unit

    (ResourcesSiftLeuronsRandom resource_id params) -> do
      eval (GetResourceId resource_id next) -- TODO FIXME
      eval (GetResourceLeuronRandom resource_id next)
      pure unit




--    (Leurons Index params) -> do
--      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
--      maybe
--        (pure unit)
--        (\(Tuple k offset) -> do
--          pageInfo <- gets _.leuronsPageInfo
--          modify (_{ leuronsPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
--          pure unit)
--        moffset
--      eval (GetLeurons next) $> unit
--
--    (Leurons New params) -> do
--      -- Important: don't over-write leuron request state.. we want to hold on to that info to make our lives easier
--      -- when adding leurons fast
--      lst <- gets _.currentLeuronRequestSt
--      modify (_{ currentLeuronRequest = Just defaultLeuronRequest, currentLeuronRequestSt = Just $ maybe defaultLeuronRequestState id lst })
--      pure unit
--
--    (Leurons (EditI leuron_id) params)   -> do
--      eval (GetLeuronId leuron_id next)
--      m_pack <- gets _.currentLeuron
--      case m_pack of
--           Nothing                          -> pure unit
--           Just (LeuronPackResponse pack) -> do
--             -- TODO FIXME: St's TyLeuronType needs to match source
--             let
--               leuron = pack.leuron ^. _LeuronResponse
--               lst    = defaultLeuronRequestState { ty = leuronToTyLeuron leuron.dataP }
--             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron, currentLeuronRequestSt = Just lst })
--             pure unit
--
--    (Leurons (DeleteI leuron_id) params) -> do
--      eval (GetLeuronId leuron_id next)
--      m_pack <- gets _.currentLeuron
--      case m_pack of
--           Nothing                          -> pure unit
--           Just (LeuronPackResponse pack) -> do
--             modify (_{ currentLeuronRequest = Just $ leuronResponseToLeuronRequest pack.leuron })
--             pure unit
--
--    (Leurons (ShowI leuron_id) params) -> eval (GetLeuronId leuron_id next) $> unit
--


    (Users Index params) -> do
      let moffset = elemBy (\(Tuple k v) -> k == "offset") params
      maybe
        (pure unit)
        (\(Tuple k offset) -> do
          pageInfo <- gets _.usersPageInfo
          modify (_{ usersPageInfo = pageInfo { currentPage = maybe 1 id (fromString offset) } })
          pure unit)
        moffset
      eval (GetUsers next) $> unit

    (Users (Show user_nick) params) -> eval (GetUser user_nick next) $> unit



    (UsersProfile user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersSettings user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersPMs user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersThreads user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersThreadPosts user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersWorkouts user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersResources user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersLeurons user_nick params) -> eval (GetUser user_nick next) $> unit
    (UsersLikes user_nick params) -> eval (GetUser user_nick next) $> unit



    _           -> pure unit



  pure next
