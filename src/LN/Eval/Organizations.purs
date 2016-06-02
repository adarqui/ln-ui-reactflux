module LN.Eval.Organizations (
  eval_GetOrganizations,
  eval_GetOrganization,
  eval_GetOrganizationForum,
  eval_GetOrganizationForumBoard,
  eval_GetOrganizationForumBoardThread
) where



import Data.Either                     (Either(..))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..))
import Halogen                         (gets, modify)
import Prelude                         (bind, pure, ($))

import LN.Api                          (rd, getOrganizationPacks')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Types                  (Input(..))
import LN.State.Loading                ( setLoading, clearLoading
                                       , l_currentOrganization, l_organizations
                                       , l_currentForum
                                       , l_currentBoard
                                       , l_currentThread)
import LN.State.Organization           (OrganizationRequestState, defaultOrganizationRequestState)
import LN.T                            ( OrganizationPackResponses(..), OrganizationPackResponse(..)
                                       , ForumPackResponse(..)
                                       , BoardPackResponse(..)
                                       , ThreadPackResponse(..))



eval_GetOrganizations :: EvalEff
eval_GetOrganizations eval (GetOrganizations next) = do

  modify (_{ organizations = (M.empty :: M.Map Int OrganizationPackResponse) })

  modify (\st->st{loading = setLoading l_organizations st.loading})

  e_organizations <- rd $ getOrganizationPacks'

  modify (\st->st{loading = clearLoading l_organizations st.loading})

  case e_organizations of

    Left err                                             -> eval (AddErrorApi "eval_GetOrganizations::getOrganizationPacks'" err next)

    Right (OrganizationPackResponses organization_packs) -> do

      let
        organizations_map =
          idmapFrom (\(OrganizationPackResponse pack) -> pack.organizationId) organization_packs.organizationPackResponses


      modify (_{ organizations = organizations_map })
      pure next



eval_GetOrganization :: EvalEff
eval_GetOrganization eval (GetOrganization org_name next) = do

  modify (_{ currentOrganization = Nothing })

-- TODO FIXME  eval (GetForumsForOrg org_name next)

  modify (\st->st{loading = setLoading l_currentOrganization st.loading})

  e_org <- rd $ ApiS.getOrganizationPack' org_name

  modify (\st->st{loading = clearLoading l_currentOrganization st.loading})
  case e_org of

    Left err   -> eval (AddErrorApi "eval_GetOrganization::ApiS.getOrganizationPack'" err next)

    Right pack -> do
      modify (_{ currentOrganization = Just pack })
      pure next



eval_GetOrganizationForum :: EvalEff
eval_GetOrganizationForum eval (GetOrganizationForum org_name forum_name next) = do

  modify (_{ currentForum = Nothing })

  modify (\st->st{loading = setLoading l_currentForum st.loading})

  e_forum <- rd $ ApiS.getForumPack_ByOrganizationName' forum_name org_name

  modify (\st->st{loading = clearLoading l_currentForum st.loading})

  case e_forum of

    Left err -> eval (AddErrorApi "eval_GetOrganizationForum::ApiS.getForumPack_ByOrganizationName'" err next)

    Right pack@(ForumPackResponse forum) -> do
      modify (_{ currentForum = Just pack })
      eval (GetBoardsForForum forum.forumId next)
      pure next



eval_GetOrganizationForumBoard :: EvalEff
eval_GetOrganizationForumBoard eval (GetOrganizationForumBoard org_name forum_name board_name next) = do

  modify (_{ currentBoard = Nothing })

  m_forum <- gets _.currentForum
  case m_forum of
       Nothing    -> eval (AddError "eval_GetOrganizationForumBoard" "Forum doesn't exist" next)
       Just forum -> go forum

  where
  go (ForumPackResponse forum_pack) = do

    modify (\st->st{loading = setLoading l_currentBoard st.loading})

    e_board <- rd $ ApiS.getBoardPack_ByForumId' board_name forum_pack.forumId

    modify (\st->st{loading = clearLoading l_currentBoard st.loading})

    case e_board of

      Left err                             -> eval (AddErrorApi "eval_GetOrganizationForumBoard::ApiS.getBoardPack_ByForumId'" err next)

      Right pack@(BoardPackResponse board) -> do
        modify (_{ currentBoard = Just pack })
        eval (GetThreadsForBoard board.boardId next)
        pure next



eval_GetOrganizationForumBoardThread :: EvalEff
eval_GetOrganizationForumBoardThread eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) = do

  modify (_{ currentThread = Nothing })

  m_board <- gets _.currentBoard
  case m_board of
    Nothing    -> eval (AddError "eval_GetOrganizationForumBoardThread" "Board doesn't exist" next)
    Just  pack -> go pack

  where
  go (BoardPackResponse pack) = do

    modify (\st->st{loading = setLoading l_currentThread st.loading})

    e_thread <- rd $ ApiS.getThreadPack_ByBoardId' thread_name pack.boardId

    modify (\st->st{loading = clearLoading l_currentThread st.loading})

    case e_thread of

      Left err                               -> eval (AddErrorApi "eval_getOrganizationForumBoardThread::ApiS.getThreadPack_ByBoardId'" err next)

      Right pack@(ThreadPackResponse thread) -> do
        modify (_{ currentThread = Just pack })
        eval (GetThreadPostsForThread thread.threadId next)
        pure next
