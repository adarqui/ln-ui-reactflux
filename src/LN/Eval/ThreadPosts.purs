module LN.Eval.ThreadPosts (
  eval_GetThreadPosts,
  eval_GetThreadPost,
  eval_GetThreadPostsForThread,
  eval_ThreadPost
) where



import Data.Array             (head, deleteAt, modifyAt, nub)
import Data.Either            (Either(..))
import Data.Functor           (($>))
import Data.Map               as M
import Data.Maybe             (Maybe(..), maybe)
import Halogen                (gets, modify)
import Optic.Core             ((^.), (..), (.~))
import Prelude                (class Eq, bind, pure, map, ($), (<>))

import LN.Api.Internal        (getThreadPostsCount_ByThreadId' , getThreadPostPacks_ByThreadId, getThreadPostPack', postThreadPost_ByThreadId', putThreadPost')
import LN.Api.Helpers         (rd)
import LN.Component.Types     (EvalEff)
import LN.Input.ThreadPost    (InputThreadPost(..), ThreadPost_Mod(..))
import LN.Input.Types         (Input(..))
import LN.Router.Class.Routes (Routes(..))
import LN.Router.Class.CRUD   (CRUD(..))
import LN.Helpers.Map         (mergeMapArray)
import LN.State.PageInfo      (runPageInfo)
import LN.T.Internal.Convert  (threadPostResponseToThreadPostRequest)
import LN.T



eval_GetThreadPosts :: EvalEff
eval_GetThreadPosts eval (GetThreadPosts next) = pure next



eval_GetThreadPost :: EvalEff
eval_GetThreadPost eval (GetThreadPost thread_post_id next) = pure next



eval_GetThreadPostsForThread :: EvalEff
eval_GetThreadPostsForThread eval (GetThreadPostsForThread thread_id next) = do

  modify (_{ threadPosts = (M.empty :: M.Map Int ThreadPostPackResponse) })

  page_info <- gets _.threadPostsPageInfo

  e_count <- rd $ getThreadPostsCount_ByThreadId' thread_id
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetThreadPostsForThread::getThreadPostsCount_ByThreadId'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { threadPostsPageInfo = new_page_info.pageInfo })

      e_posts <- rd $ getThreadPostPacks_ByThreadId new_page_info.params thread_id
      case e_posts of
        Left err -> eval (AddErrorApi "eval_GetThreadPostsForThread::getThreadPostPacks_ByThreadId" err next)
        Right (ThreadPostPackResponses posts) -> do

          let
            users_ids =
              map (\thread_post_pack -> thread_post_pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. userId_) posts.threadPostPackResponses

          eval (GetUsers_MergeMap_ByUserId users_ids next)

          modify (\st -> st{
                 threadPosts =
                   mergeMapArray
                     st.threadPosts
                     posts.threadPostPackResponses
                     (\x -> x ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_)
                 })
          pure next



eval_ThreadPost :: EvalEff
eval_ThreadPost eval (CompThreadPost sub next) = do
  org_pack    <- gets _.currentOrganization
  forum_pack  <- gets _.currentForum
  board_pack  <- gets _.currentBoard
  thread_pack <- gets _.currentThread
  let
    org_name    = maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) org_pack
    forum_name  = maybe "unknown" (\forum -> forum ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. name_) forum_pack
    board_name  = maybe "unknown" (\board -> board ^. _BoardPackResponse .. board_ ^. _BoardResponse .. name_) board_pack
    thread_name = maybe "unknown" (\thread -> thread ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse .. name_) thread_pack

  case sub of
    InputThreadPost_Mod q -> do
      case q of
--        SetDisplayName name -> mod $ set (\req -> _ThreadPostRequest .. displayName_ .~ name $ req)
        SetBody text -> mod $ set (\req -> _ThreadPostRequest .. body_ .~ PostDataBBCode text $ req)

        Create thread_id     -> do

          m_req <- gets _.currentThreadPostRequest

          case m_req of

               Just req -> do

                 e_post <- rd $ postThreadPost_ByThreadId' thread_id req

                 case e_post of
                      Left err                        -> eval (AddErrorApi "eval_ThreadPost(Create)::postThreadPost'" err next)
                      Right (ThreadPostResponse post) -> eval (Goto (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI post.id) []) next)


               _        -> eval (AddError "eval_ThreadPost(Create)" "ThreadPost request doesn't exist" next)



        EditP thread_post_id -> do

          m_req <- gets _.currentThreadPostRequest

          case m_req of
               Nothing  -> eval (AddError "eval_ThreadPost(Edit)" "ThreadPost request doesn't exist" next)
               Just req -> do

                 e_post <- rd $ putThreadPost' thread_post_id req

                 case e_post of
                      Left err  -> eval (AddErrorApi "eval_ThreadPost(Edit)::putThreadPost" err next)
                      Right post -> do

                        modify (\st->st{ currentThreadPostRequest = Just $ threadPostResponseToThreadPostRequest post })
                        eval (Goto (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI thread_post_id) []) next)
                        pure next

    _   -> pure next

 where
 append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
 append Nothing a    = Just [a]
 append (Just arr) a = Just $ nub $ arr <> [a]
 set v req           = Just (v req)
 mod new             = modify (\st->st{ currentThreadPostRequest = maybe Nothing new st.currentThreadPostRequest }) $> next
