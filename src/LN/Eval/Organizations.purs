module LN.Eval.Organizations (
  eval_GetOrganizations,
  eval_GetOrganization,
  eval_GetOrganizationForum,
  eval_GetOrganizationForumBoard,
  eval_GetOrganizationForumBoardThread
) where



import Control.Monad.Aff.Console       (log)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (get, gets, modify, liftAff')
import Prelude                         (bind, pure, show, ($), (<>))

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

    Left err -> liftAff' (log $ "eval_GetOrganizations: " <> show err) $> next

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

    Left err   -> liftAff' (log $ "eval_GetOrganization: " <> show err) $> next

    Right pack -> do
      modify (_{ currentOrganization = Just pack })
      pure next



eval_GetOrganizationForum :: EvalEff
eval_GetOrganizationForum eval (GetOrganizationForum org_name forum_name next) = do

  e_forum <- rd $ ApiS.getForumPack_ByOrganizationName' forum_name org_name
  case e_forum of

    Left err -> liftAff' (log $ "eval_GetOrganizationForum: " <> show err) $> next

    Right pack@(ForumPackResponse forum) -> do
      modify (_{ currentForum = Just pack })
      eval (GetBoardsForForum forum.forumId next)
      pure next



eval_GetOrganizationForumBoard :: EvalEff
eval_GetOrganizationForumBoard eval (GetOrganizationForumBoard org_name forum_name board_name next) = do

  st <- get
  let forum_id = maybe 0 (\(ForumPackResponse forum) -> forum.forumId) st.currentForum

  e_board <- rd $ ApiS.getBoardPack_ByForumId' board_name forum_id
  case e_board of

    Left err -> liftAff' (log $ "eval_GetOrganizationForumBoard: " <> show err) $> next

    Right pack@(BoardPackResponse board) -> do
      modify (_{ currentBoard = Just pack })
      eval (GetThreadsForBoard board.boardId next)
      pure next



eval_GetOrganizationForumBoardThread :: EvalEff
eval_GetOrganizationForumBoardThread eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) = do

  m_board <- gets _.currentBoard
  case m_board of
    Nothing    -> liftAff' (log $ "eval_GetOrganizationForumBoardThread: No currentBoard") $> next
    Just  pack -> go pack

  where
  go (BoardPackResponse pack) = do
    e_thread <- rd $ ApiS.getThreadPack_ByBoardId' thread_name pack.boardId
    case e_thread of

      Left err -> liftAff' (log $ "eval_getOrganizationForumBoardThread: " <> show err) $> next

      Right pack@(ThreadPackResponse thread) -> do
        modify (_{ currentThread = Just pack })
        eval (GetThreadPostsForThread thread.threadId next)
        pure next
