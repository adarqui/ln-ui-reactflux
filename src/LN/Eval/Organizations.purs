module LN.Eval.Organizations (
  eval_GetOrganizations,
  eval_GetOrganization,
  eval_GetOrganizationForum,
  eval_GetOrganizationForumBoard,
  eval_GetOrganizationForumBoardThread
) where



import Data.Either                     (Either(..))
import Data.Maybe                      (Maybe(..))
import Halogen                         (gets, modify)
import Prelude                         (bind, pure, ($))

import LN.Api                          (rd, getOrganizationPacks')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Types                  (Input(..))
import LN.T                            ( OrganizationPackResponses(..), OrganizationPackResponse(..)
                                       , ForumPackResponse(..)
                                       , BoardPackResponse(..)
                                       , ThreadPackResponse(..))



eval_GetOrganizations :: EvalEff
eval_GetOrganizations eval (GetOrganizations next) = do

  e_organizations <- rd $ getOrganizationPacks'
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

  eval (GetForumsForOrg org_name next)

  e_org <- rd $ ApiS.getOrganizationPack' org_name
  case e_org of

    Left err   -> eval (AddErrorApi "eval_GetOrganization::ApiS.getOrganizationPack'" err next)

    Right pack -> do
      modify (_{ currentOrganization = Just pack })
      pure next



eval_GetOrganizationForum :: EvalEff
eval_GetOrganizationForum eval (GetOrganizationForum org_name forum_name next) = do

  e_forum <- rd $ ApiS.getForumPack_ByOrganizationName' forum_name org_name
  case e_forum of

    Left err -> eval (AddErrorApi "eval_GetOrganizationForum::ApiS.getForumPack_ByOrganizationName'" err next)

    Right pack@(ForumPackResponse forum) -> do
      modify (_{ currentForum = Just pack })
      eval (GetBoardsForForum forum.forumId next)
      pure next



eval_GetOrganizationForumBoard :: EvalEff
eval_GetOrganizationForumBoard eval (GetOrganizationForumBoard org_name forum_name board_name next) = do

  m_forum <- gets _.currentForum
  case m_forum of
       Nothing    -> eval (AddError "eval_GetOrganizationForumBoard" "Forum doesn't exist" next)
       Just forum -> go forum

  where
  go (ForumPackResponse forum_pack) = do

    e_board <- rd $ ApiS.getBoardPack_ByForumId' board_name forum_pack.forumId
    case e_board of

      Left err                             -> eval (AddErrorApi "eval_GetOrganizationForumBoard::ApiS.getBoardPack_ByForumId'" err next)

      Right pack@(BoardPackResponse board) -> do
        modify (_{ currentBoard = Just pack })
        eval (GetThreadsForBoard board.boardId next)
        pure next



eval_GetOrganizationForumBoardThread :: EvalEff
eval_GetOrganizationForumBoardThread eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) = do

  m_board <- gets _.currentBoard
  case m_board of
    Nothing    -> eval (AddError "eval_GetOrganizationForumBoardThread" "Board doesn't exist" next)
    Just  pack -> go pack

  where
  go (BoardPackResponse pack) = do
    e_thread <- rd $ ApiS.getThreadPack_ByBoardId' thread_name pack.boardId
    case e_thread of

      Left err                               -> eval (AddErrorApi "eval_getOrganizationForumBoardThread::ApiS.getThreadPack_ByBoardId'" err next)

      Right pack@(ThreadPackResponse thread) -> do
        modify (_{ currentThread = Just pack })
        eval (GetThreadPostsForThread thread.threadId next)
        pure next
