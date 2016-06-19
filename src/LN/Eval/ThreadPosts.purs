module LN.Eval.ThreadPosts (
  eval_ThreadPost
) where



import Data.Array               (head, deleteAt, modifyAt, nub, sort, (:))
import Data.Ebyam               (ebyam)
import Data.Either              (Either(..))
import Data.Functor             (($>))
import Data.Map                 as M
import Data.Maybe               (Maybe(..), maybe)
import Data.String              (toLower)
import Halogen                  (gets, modify)
import Optic.Core               ((^.), (..), (.~))
import Prelude                  (class Eq, id, bind, pure, map, const, ($), (<>), (<<<))

import LN.Api.Internal          (getThreadPostsCount_ByThreadId' , getThreadPostPacks_ByThreadId, getThreadPostPack', postThreadPost_ByThreadId', putThreadPost')
import LN.Api.Internal.String   as ApiS
import LN.Api.Helpers           (rd)
import LN.Component.Types       (EvalEff)
import LN.Input.ThreadPost      (InputThreadPost(..), ThreadPost_Act(..), ThreadPost_Mod(..))
import LN.Input.Types           (Input(..))
import LN.Router.Class.Routes   (Routes(..))
import LN.Router.Class.CRUD     (CRUD(..))
import LN.Router.Class.Params   (emptyParams)
import LN.Helpers.Map           (mergeMapArray)
import LN.State.Loading         (l_currentThreadPost, l_threadPosts)
import LN.State.Loading.Helpers (setLoading, clearLoading)
import LN.State.PageInfo        (runPageInfo)
import LN.T.Internal.Convert    (threadPostResponseToThreadPostRequest)
import LN.T



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
    InputThreadPost_Act q -> do
      case q of
        Gets_ByCurrentThread                          -> act_gets_by_current_thread
        Gets_ByCurrentThread_And_ThreadPostId post_id -> act_gets_by_current_thread_and_thread_post_id post_id
        GetId thread_post_id                          -> act_get_id thread_post_id
        ReplaceById thread_post_id                    -> act_replace_by_id thread_post_id

    InputThreadPost_Mod q -> do
      case q of
        SetBody text          -> mod $ set (\req -> _ThreadPostRequest .. body_ .~ PostDataBBCode text $ req)

        SetPrivateTag s       -> modSt $ (_{currentPrivateTag = Just s})
        AddPrivateTag         -> do
          m_req_st <- gets _.currentThreadPostRequestSt
          ebyam m_req_st (pure next) $ \req_st ->
            case req_st.currentPrivateTag of
               Nothing  -> pure next
               Just tag -> do
                 mod $ set (\(ThreadPostRequest req) -> ThreadPostRequest req{privateTags = nub $ sort $ toLower tag : req.privateTags})
                 modSt $ (_{currentPrivateTag = Nothing})
                 pure next
        DeletePrivateTag idx   -> mod $ set (\(ThreadPostRequest req) -> ThreadPostRequest req{privateTags = maybe req.privateTags id $ deleteAt idx req.privateTags})
        ClearPrivateTags       -> mod $ set (\req -> _ThreadPostRequest .. privateTags_ .~ [] $ req)

        SetTag s               -> modSt $ (_{currentTag = Just s})
        AddTag                 -> do
          m_req_st <- gets _.currentThreadPostRequestSt
          ebyam m_req_st (pure next) $ \req_st ->
            case req_st.currentTag of
               Nothing  -> pure next
               Just tag -> do
                 mod $ set (\(ThreadPostRequest req) -> ThreadPostRequest req{tags = nub $ sort $ toLower tag : req.tags})
                 modSt $ (_{currentTag = Nothing})
                 pure next
        DeleteTag idx          -> mod $ set (\(ThreadPostRequest req) -> ThreadPostRequest req{tags = maybe req.tags id $ deleteAt idx req.tags})
        ClearTags              -> mod $ set (\req -> _ThreadPostRequest .. tags_ .~ [] $ req)

        Create thread_id       -> mod_create thread_id org_name forum_name board_name thread_name
        EditP thread_post_id   -> mod_edit thread_post_id org_name forum_name board_name thread_name

    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentThreadPostRequest = maybe Nothing new st.currentThreadPostRequest }) $> next
  modSt new           = modify (\st->st{ currentThreadPostRequestSt = maybe Nothing (Just <<< new) st.currentThreadPostRequestSt }) $> next



  act_gets_by_current_thread = do
    modify (_{ threadPosts = (M.empty :: M.Map Int ThreadPostPackResponse) })
    m_thread_pack <- gets _.currentThread
    case m_thread_pack of
      Nothing          -> eval (AddError "eval_ThreadPost(Act/Gets)" "Thread doesn't exist" next)
      Just thread_pack -> do
        page_info <- gets _.threadPostsPageInfo
        e_count <- rd $ getThreadPostsCount_ByThreadId' (thread_pack ^. _ThreadPackResponse .. threadId_)
        case e_count of
          Left err     -> eval (AddErrorApi "eval_ThreadPost(Act/Gets)::getThreadPostsCount_ByThreadId'" err next)
          Right counts -> do
            let new_page_info = runPageInfo counts page_info
            modify (_ { threadPostsPageInfo = new_page_info.pageInfo })
            modify $ setLoading l_currentThreadPost
            e_posts <- rd $ getThreadPostPacks_ByThreadId new_page_info.params (thread_pack ^. _ThreadPackResponse .. threadId_)
            modify $ clearLoading l_currentThreadPost
            case e_posts of
              Left err -> eval (AddErrorApi "eval_ThreadPost(Act/Gets)::getThreadPostPacks_ByThreadId" err next)
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



  act_gets_by_current_thread_and_thread_post_id post_id = do
    -- TODO FIXME
    act_gets_by_current_thread
    pure next



  act_get_id thread_post_id = do
    modify (_{ currentThreadPost = Nothing })
    modify $ setLoading l_currentThreadPost
    e_thread_pack <- rd $ getThreadPostPack' thread_post_id
    modify $ clearLoading l_currentThreadPost
    case e_thread_pack of
      Left err         -> eval (AddErrorApi "eval_ThreadPost(Act/Get)::getThreadPostPack'" err next)
      Right thread_pack -> do
        modify (_{ currentThreadPost = Just thread_pack })
        pure next



  act_replace_by_id thread_post_id = do
    thread_posts <- gets _.threadPosts
    if M.member thread_post_id thread_posts
       then do
         e_pack <- rd $ getThreadPostPack' thread_post_id
         case e_pack of
          Left _     -> pure next
          Right pack -> do
            modify (\st->st{threadPosts = M.update (const $ Just pack) thread_post_id thread_posts})
            pure next
       else pure next



  mod_create thread_id org_name forum_name board_name thread_name = do
    m_req <- gets _.currentThreadPostRequest
    case m_req of
      Just req -> do
        e_post <- rd $ postThreadPost_ByThreadId' thread_id req
        case e_post of
          Left err                        -> eval (AddErrorApi "eval_ThreadPost(Create)::postThreadPost'" err next)
          Right (ThreadPostResponse post) -> eval (Goto (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI post.id) emptyParams) next)
      _        -> eval (AddError "eval_ThreadPost(Create)" "ThreadPost request doesn't exist" next)



  mod_edit thread_post_id org_name forum_name board_name thread_name = do
    m_req <- gets _.currentThreadPostRequest
    case m_req of
       Nothing  -> eval (AddError "eval_ThreadPost(Edit)" "ThreadPost request doesn't exist" next)
       Just req -> do
         e_post <- rd $ putThreadPost' thread_post_id req
         case e_post of
           Left err  -> eval (AddErrorApi "eval_ThreadPost(Edit)::putThreadPost" err next)
           Right post -> do
             modify (\st->st{ currentThreadPostRequest = Just $ threadPostResponseToThreadPostRequest post })
             eval (Goto (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI thread_post_id) emptyParams) next)
             pure next
