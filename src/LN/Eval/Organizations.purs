module LN.Eval.Organizations (
  eval_GetOrganizations,
  eval_GetOrganization,
  eval_GetOrganizationForum,
  eval_GetOrganizationForumBoard,
  eval_GetOrganizationForumBoardThread
) where



import Control.Monad.Aff.Console       (log)
import Data.Array                      (zip)
import Data.Either                     (Either(..))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (get, modify, liftAff')
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, map, show, ($), (<>))

import LN.Api                          (rd, getOrganizationPacks')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T



eval_GetOrganizations :: EvalEff
eval_GetOrganizations eval (GetOrganizations next) = do

  eorganizations <- rd $ getOrganizationPacks'
  case eorganizations of

    Left err -> do
      liftAff' $ log ("organizations error: " <> (show err))
      pure next

    Right (OrganizationPackResponses organization_packs) -> do

      let
        organizations     = organization_packs.organizationPackResponses
        organizations_map = M.fromFoldable $ zip (map (\(OrganizationPackResponse pack) -> pack.organization ^. _OrganizationResponse .. id_) organizations) organizations

      modify (_{ organizations = organizations_map })
      pure next



eval_GetOrganization :: EvalEff
eval_GetOrganization eval (GetOrganization org_name next) = do

  eval (GetForumsForOrg org_name next)

  eorg <- rd $ ApiS.getOrganizationPack' org_name
  case eorg of
    Left err -> pure next
    Right pack -> do
      modify (_{ currentOrganization = Just pack })
      pure next



eval_GetOrganizationForum :: EvalEff
eval_GetOrganizationForum eval (GetOrganizationForum org_name forum_name next) = do

  eforum <- rd $ ApiS.getForumPack_ByOrganizationName' forum_name org_name
  case eforum of
    Left err -> pure next
    Right pack@(ForumPackResponse forum) -> do
      modify (_{ currentForum = Just pack })
      -- IMPLEMENTING BOARD PACKS
      eval (GetBoardsForForum forum.forumId next)
      pure next



eval_GetOrganizationForumBoard :: EvalEff
eval_GetOrganizationForumBoard eval (GetOrganizationForumBoard org_name forum_name board_name next) = do

  st <- get
  let forum_id = maybe 0 (\(ForumPackResponse forum) -> forum.forumId) st.currentForum

  eboard <- rd $ ApiS.getBoardPack_ByForumId' board_name forum_id
  case eboard of
    Left err -> pure next
    Right pack@(BoardPackResponse board) -> do
      modify (_{ currentBoard = Just pack })
      eval (GetThreadsForBoard board.boardId next)
      pure next



eval_GetOrganizationForumBoardThread :: EvalEff
eval_GetOrganizationForumBoardThread eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) = do

  st <- get
  let board_id = maybe 0 (\(BoardPackResponse board) -> board.boardId) st.currentBoard

  ethread <- rd $ ApiS.getThreadPack_ByBoardId' thread_name board_id
  case ethread of
    Left err -> pure next
    Right pack@(ThreadPackResponse thread) -> do
      modify (_{ currentThread = Just pack })
      eval (GetThreadPostsForThread thread.threadId next)
      pure next
