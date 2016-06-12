module LN.Eval.Threads (
  eval_Thread
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, catMaybes)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, bind, pure, map, ($), (<>))

import LN.Api                          (getThreadPacks_ByBoardId, rd, getThreadsCount_ByBoardId', getThreadPack', postThread_ByBoardId', putThread')
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Thread                 (InputThread(..), Thread_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.State.Loading                (l_currentThread)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.State.PageInfo               (runPageInfo)
import LN.T.Internal.Convert           (threadResponseToThreadRequest)
import LN.T



{-
eval_GetThreads :: EvalEff
eval_GetThreads eval (GetThreads next) = pure next



eval_GetThreadsForBoard :: EvalEff
eval_GetThreadsForBoard eval (GetThreadsForBoard board_id next) = do

  modify (_{ threads = (M.empty :: M.Map Int ThreadPackResponse) })

  page_info <- gets _.threadsPageInfo

  e_count <- rd $ getThreadsCount_ByBoardId' board_id
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetThreadsForBoard::getThreadsCount_ByBoardId'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { threadsPageInfo = new_page_info.pageInfo })

      e_thread_packs <- rd $ getThreadPacks_ByBoardId new_page_info.params board_id
      case e_thread_packs of
        Left err -> pure next
        Right (ThreadPackResponses thread_packs) -> do

          let
            users =
              (catMaybes $ map (\(ThreadPackResponse pack) -> pack.latestThreadPostUser) thread_packs.threadPackResponses)
              <>
              (map (\(ThreadPackResponse pack) -> pack.user) thread_packs.threadPackResponses)

            threads_map = idmapFrom (\(ThreadPackResponse p) -> p.threadId) thread_packs.threadPackResponses

          eval (GetUsers_MergeMap_ByUser users next)

          modify (_{ threads = threads_map })
          pure next
          -}



eval_Thread :: EvalEff
eval_Thread eval (CompThread sub next) = do

  org_pack   <- gets _.currentOrganization
  forum_pack <- gets _.currentForum
  board_pack <- gets _.currentBoard
  let
    org_name   = maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) org_pack
    forum_name = maybe "unknown" (\org -> org ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. name_) forum_pack
    board_name = maybe "unknown" (\org -> org ^. _BoardPackResponse .. board_ ^. _BoardResponse .. name_) board_pack

  case sub of
    InputThread_Mod q -> do
      case q of
        SetDisplayName name -> mod $ set (\req -> _ThreadRequest .. displayName_ .~ name $ req)

        SetDescription s    -> mod $ set (\req -> _ThreadRequest .. description_ .~ Just s $ req)
        RemoveDescription   -> mod $ set (\req -> _ThreadRequest .. description_ .~ Nothing $ req)

        SetIcon s           -> mod $ set (\req -> _ThreadRequest .. icon_ .~ Just s $ req)
        RemoveIcon          -> mod $ set (\req -> _ThreadRequest .. icon_ .~ Nothing $ req)

        Create board_id     -> do

          m_req <- gets _.currentThreadRequest

          case m_req of

               Just req -> do

                 e_thread <- rd $ postThread_ByBoardId' board_id req

                 case e_thread of
                      Left err                      -> eval (AddErrorApi "eval_Thread(Create)::postThread'" err next)
                      Right (ThreadResponse thread) -> eval (Goto (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread.name) []) next)


               _        -> eval (AddError "eval_Thread(Create)" "Thread request doesn't exist" next)


        EditP thread_id -> do

          m_req <- gets _.currentThreadRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Thread(Edit)" "Thread request doesn't exist" next)
               Just req -> do

                 e_thread <- rd $ putThread' thread_id req

                 case e_thread of
                      Left err     -> eval (AddErrorApi "eval_Thread(Edit)::putThread" err next)
                      Right thread -> do

                        modify (\st->st{ currentThreadRequest = Just $ threadResponseToThreadRequest thread })
                        eval (Goto (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show $ thread ^. _ThreadResponse .. name_) []) next)
                        pure next

    _   -> pure next

 where
 append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
 append Nothing a    = Just [a]
 append (Just arr) a = Just $ nub $ arr <> [a]
 set v req           = Just (v req)
 mod new             = modify (\st->st{ currentThreadRequest = maybe Nothing new st.currentThreadRequest }) $> next
