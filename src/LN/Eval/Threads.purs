module LN.Eval.Threads (
  eval_GetThreads,
  eval_GetThreadsForBoard
) where


import Halogen                         (gets, modify)
import Data.Array                      (catMaybes)
import Data.Either                     (Either(..))
import Prelude                         (bind, pure, map, ($), (<>))

import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.State.PageInfo               (runPageInfo)
import LN.Api                          (getThreadPacks_ByBoardId, rd, getThreadsCount_ByBoardId'
                                       , getThreadPacks')
import LN.T



eval_GetThreads :: EvalEff
eval_GetThreads eval (GetThreads next) = do

  ethreadPacks <- rd $ getThreadPacks'
  case ethreadPacks of
    Left err -> pure next
    Right (ThreadPackResponses threadPacks) -> do
      modify (_{ threads = threadPacks.threadPackResponses })
      pure next



eval_GetThreadsForBoard :: EvalEff
eval_GetThreadsForBoard eval (GetThreadsForBoard board_id next) = do

  page_info <- gets _.threadsPageInfo

  ecount <- rd $ getThreadsCount_ByBoardId' board_id
  case ecount of
    Left err -> pure next
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { threadsPageInfo = new_page_info.pageInfo })

      ethreadPacks <- rd $ getThreadPacks_ByBoardId new_page_info.params board_id
      case ethreadPacks of
        Left err -> pure next
        Right (ThreadPackResponses threadPacks) -> do

          let
            users =
              (catMaybes $ map (\(ThreadPackResponse pack) -> pack.latestThreadPostUser) threadPacks.threadPackResponses)
              <>
              (map (\(ThreadPackResponse pack) -> pack.threadUser) threadPacks.threadPackResponses)

          eval (GetUsers_MergeMap_ByUser users next)

          modify (_{ threads = threadPacks.threadPackResponses })
          pure next
