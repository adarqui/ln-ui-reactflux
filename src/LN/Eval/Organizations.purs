module LN.Eval.Organizations (
  eval_GetOrganizations,
  eval_GetOrganization,
  eval_GetOrganizationForum,
  eval_GetOrganizationForumBoard,
  eval_GetOrganizationForumBoardThread
) where



import Control.Monad.Aff.Console       (log)
import Data.Array                      (length)
import Data.Either                     (Either(..))
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (get, modify, liftAff')
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, show, ($), (<>))

import LN.Api                          (rd, getOrganizations')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T



eval_GetOrganizations :: EvalEff
eval_GetOrganizations eval (GetOrganizations next) = do

  eorganizations <- rd $ getOrganizations'
  case eorganizations of
    Left err -> do
      liftAff' $ log ("organizations error: " <> (show err))
      pure next
    Right (OrganizationResponses organizations) -> do
      modify (_{ organizations = organizations.organizationResponses })
      liftAff' $ log ("wtf " <> show (length organizations.organizationResponses))
      pure next



eval_GetOrganization :: EvalEff
eval_GetOrganization eval (GetOrganization org_name next) = do

  eval (GetForumsForOrg org_name next)

  eorg <- rd $ ApiS.getOrganization' org_name
  case eorg of
    Left err -> pure next
    Right org -> do
      modify (_{ currentOrganization = Just org })
      pure next



eval_GetOrganizationForum :: EvalEff
eval_GetOrganizationForum eval (GetOrganizationForum org_name forum_name next) = do

  eforum <- rd $ ApiS.getForum_ByOrganizationName' forum_name org_name
  case eforum of
    Left err -> pure next
    Right forum@(ForumResponse f) -> do
      modify (_{ currentForum = Just forum })
      -- IMPLEMENTING BOARD PACKS
      eval (GetBoardsForForum f.id next)
      pure next



eval_GetOrganizationForumBoard :: EvalEff
eval_GetOrganizationForumBoard eval (GetOrganizationForumBoard org_name forum_name board_name next) = do

  st <- get
  let forum_id = maybe 0 (\forum -> forum ^. _ForumResponse .. id_) st.currentForum

  eboard <- rd $ ApiS.getBoard_ByForumId' board_name forum_id
  case eboard of
    Left err -> pure next
    Right board@(BoardResponse b) -> do
      modify (_{ currentBoard = Just board })
-- IMPLEMENTING THREADS PACKS
      eval (GetThreadsForBoard b.id next)
      pure next



eval_GetOrganizationForumBoardThread :: EvalEff
eval_GetOrganizationForumBoardThread eval (GetOrganizationForumBoardThread org_name forum_name board_name thread_name next) = do

  st <- get
  let board_id = maybe 0 (\board -> board ^. _BoardResponse .. id_) st.currentBoard

  ethread <- rd $ ApiS.getThread_ByBoardId' thread_name board_id
  case ethread of
    Left err -> pure next
    Right thread@(ThreadResponse t) -> do
      modify (_{ currentThread = Just thread })
      eval (GetThreadPostsForThread t.id next)
      pure next
