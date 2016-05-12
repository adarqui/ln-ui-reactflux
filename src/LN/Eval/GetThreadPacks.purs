module LN.Eval.GetThreadPacks (
  eval_GetThreadPacks,
  eval_GetThreadPacksForBoard
) where


import Halogen                         (get, gets, modify)
import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Array                      hiding ((..))
import Prelude                         (bind, pure, show, ($), (<>))

import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.Api
import LN.T



eval_GetThreadPacks :: EvalEff
eval_GetThreadPacks eval (GetThreadPacks next) = do

  ethreadPacks <- rd $ getThreadPacks'
  case ethreadPacks of
    Left err -> pure next
    Right (ThreadPackResponses threadPacks) -> do
      modify (_{ threadPacks = threadPacks.threadPackResponses })
      pure next



eval_GetThreadPacksForBoard :: EvalEff
eval_GetThreadPacksForBoard eval (GetThreadPacksForBoard board_id next) = do

  pageInfo <- gets _.threadsPageInfo

  ecount <- rd $ getThreadsCount_ByBoardId' board_id
  case ecount of
    Left err -> pure next
    Right (CountResponses counts) -> do

      let count = maybe 0 (\(CountResponse count) -> count.n) (head counts.countResponses)

      modify (_ { threadsPageInfo = pageInfo { totalResults = count, totalPages = (count / pageInfo.resultsPerPage)+1 } })

      ethreadPacks <- rd $ getThreadPacks_ByBoardId [Limit pageInfo.resultsPerPage, Offset ((pageInfo.currentPage-1) * pageInfo.resultsPerPage), SortOrder pageInfo.sortOrder, Order pageInfo.order] board_id
      case ethreadPacks of
        Left err -> pure next
        Right (ThreadPackResponses threadPacks) -> do

          let
            users =
              (catMaybes $ map (\(ThreadPackResponse pack) -> pack.latestThreadPostUser) threadPacks.threadPackResponses)
              <>
              (map (\(ThreadPackResponse pack) -> pack.threadUser) threadPacks.threadPackResponses)

          eval (GetUsers_MergeMap_ByUser users next)

          modify (_{ threadPacks = threadPacks.threadPackResponses })
          pure next
