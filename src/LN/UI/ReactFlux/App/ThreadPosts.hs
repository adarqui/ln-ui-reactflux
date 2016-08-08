{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.ThreadPosts (
  viewIndex,
  viewIndex_,
  viewNew,
  viewEditI,
  viewShowI
) where



import           Control.Concurrent                   (forkIO)
import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (forM_, void)
import           Control.Monad.Trans.Either           (EitherT, runEitherT)
import           Data.Ebyam                           (ebyam)
import           Data.Int                             (Int64)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Monoid                          ((<>))
import           Data.Rehtie                          (rehtie)
import           Data.Text                            (Text)
import           Data.Tuple.Select
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           Haskell.Helpers.Either               (mustPassT)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.Api
import qualified LN.Api.String                        as ApiS
import           LN.Generate.Default                  (defaultBoardRequest)
import           LN.T.Board
import           LN.T.Convert
import           LN.T.Forum
import           LN.T.Organization
import           LN.T.Pack.Board
import           LN.T.Pack.Forum
import           LN.T.Pack.Organization
import           LN.T.Pack.Sanitized.User
import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Profile
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import           LN.UI.Core.Access
import qualified LN.UI.Core.App.ThreadPost            as ThreadPost
import           LN.UI.Core.Helpers.DataList          (deleteNth)
import           LN.UI.Core.Helpers.DataText          (tshow)
import           LN.UI.Core.Helpers.DataTime          (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Core.Helpers.Map               (idmapFrom)
import           LN.UI.Core.PageInfo                  (PageInfo (..),
                                                       defaultPageInfo,
                                                       pageInfoFromParams,
                                                       paramsFromPageInfo)
import           LN.UI.Core.Router                    (CRUD (..), Params,
                                                       Route (..),
                                                       RouteWith (..),
                                                       TyCRUD (..), emptyParams,
                                                       linkName, routeWith,
                                                       routeWith')
import           LN.UI.Core.Sort
import           LN.UI.ReactFlux.Access
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete           as Delete
import qualified LN.UI.ReactFlux.App.Gravatar         as Gravatar
import           LN.UI.ReactFlux.App.Loader           (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader           as Loader
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound (view_)
import qualified LN.UI.ReactFlux.App.Oops             as Oops (view_)
import           LN.UI.ReactFlux.App.PageNumbers      (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers      as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (targetValue)
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefClasses,
                                                       ahrefClassesName,
                                                       ahrefName, className_,
                                                       classNames_)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Button          (showBadge)
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal        (showTagsSmall)




viewIndex
  :: PageInfo
  -> UserId
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Map ThreadPostId ThreadPostPackResponse)
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewIndex page_info me_id l_m_organization l_m_forum l_m_board l_m_thread l_posts m_request users_map = do
  h1_ [className_ B.textCenter] $ elemText "Posts"
  Loader.maybeLoader4 l_m_organization l_m_forum l_m_board l_m_thread $ \organization forum board thread -> do
    Loader.loader1 l_posts $ \posts -> do
      viewIndex_ page_info me_id organization forum board thread posts m_request users_map



viewIndex_
  :: PageInfo
  -> UserId
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> Map ThreadPostId ThreadPostPackResponse
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewIndex_ page_info me_id organization forum board thread posts m_request users_map = do

  viewShared
    page_info
    me_id
    organization
    forum
    board
    thread
    posts
    m_request
    users_map



viewShowI
  :: PageInfo
  -> UserId
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Maybe ThreadPostPackResponse)
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShowI page_info me_id l_m_organization l_m_forum l_m_board l_m_thread l_m_post users_map = do
  Loader.maybeLoader5 l_m_organization l_m_forum l_m_board l_m_thread l_m_post $ \organization forum board thread post -> do
    viewShowI_
      page_info
      me_id
      organization
      forum
      board
      thread
      post
      users_map



viewShowI_
  :: PageInfo
  -> UserId
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> ThreadPostPackResponse
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShowI_ page_info me_id organization forum board thread post users_map = do
  case Map.lookup (userSanitizedResponseId $ threadPostPackResponseUser post) users_map of
    Nothing   -> p_ $ elemText "User not found in Map."
    Just user -> viewShowI__ page_info me_id organization forum board thread post user



viewShowI__
  :: PageInfo
  -> UserId
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> ThreadPostPackResponse
  -> UserSanitizedPackResponse
  -> HTMLView_

viewShowI__ page_info me_id organization forum board thread post user = do
  cldiv_ B.row $ do
    cldiv_ B.colXs2 $ do
      ahref $ routeWith' $ Users (ShowS userSanitizedResponseName)
      Gravatar.viewUser_ Medium userSanitizedPackResponseUser
      viewUserStats user
    cldiv_ B.colXs8 $ do
      ahrefName (threadResponseName <> "/" <> tshow threadPostResponseId) $ routeWith' $ OrganizationsForumsBoardsThreadsPosts organizationResponseName forumResponseName boardResponseName threadResponseName (ShowI threadPostResponseId)
      p_ $ elemText (prettyUTCTimeMaybe threadPostResponseCreatedAt)
      p_ $ elemText "quote / reply"

      -- white-space: pre ... for proper output of multiple spaces etc
      div_ [ "style" @= (Map.fromList [("text-white-space", "whitespace-pre")] :: Map Text Text)
           ] $ viewPostData threadPostResponseBody

      p_ $ elemText $ maybe "" id profileResponseSignature

    cldiv_ B.colXs1 $ do
      buttonGroup_VerticalSm1 $ do
        -- ACCESS: ThreadPost
        -- * Update: edit thread post
        -- * Delete: delete thread post
        --
        permissionsHTML'
          threadPostPackResponsePermissions
          permCreateEmpty
          permReadEmpty
          (button_editThreadPost $ routeWith' $ OrganizationsForumsBoardsThreadsPosts organizationResponseName forumResponseName boardResponseName threadResponseName (EditI threadPostResponseId))
          (button_deleteThreadPost $ routeWith' $ OrganizationsForumsBoardsThreadsPosts organizationResponseName forumResponseName boardResponseName threadResponseName (DeleteI threadPostResponseId))
          permExecuteEmpty

      cldiv_ B.colXs1 $ do
        -- ACCESS: Member & Not self
        -- Member: must be a member to like/star
        -- Not Self: can't like/star your own posts
        if orgMember organization && notSelf me_id threadPostResponseUserId
          then p_ $ elemText "like.." -- TODO FIXME: renderLike Ent_ThreadPost post.id like star
          else mempty
        -- TODO FIXME: displayPostStats threadPostPackStats

  where
  OrganizationPackResponse{..}  = organization
  OrganizationResponse{..}      = organizationPackResponseOrganization
  ForumPackResponse{..}         = forum
  ForumResponse{..}             = forumPackResponseForum
  BoardPackResponse{..}         = board
  BoardResponse{..}             = boardPackResponseBoard
  ThreadPackResponse{..}        = thread
  ThreadResponse{..}            = threadPackResponseThread
  ThreadPostPackResponse{..}    = post
  ThreadPostResponse{..}        = threadPostPackResponseThreadPost
  UserSanitizedPackResponse{..} = user
  UserSanitizedResponse{..}     = userSanitizedPackResponseUser
  ProfileResponse{..}           = userSanitizedPackResponseProfile




viewShared
  :: PageInfo
  -> UserId
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> Map ThreadPostId ThreadPostPackResponse
  -> Maybe ThreadPostRequest
  -> Map UserId UserSanitizedPackResponse
  -> HTMLView_

viewShared
  page_info
  me_id
  organization
  forum
  board
  thread
  posts
  m_request
  users_map
  =
  div_ $ do
    PageNumbers.view_ (page_info, routeWith' Home)
    ul_ [className_ B.listUnstyled] $ do
      forM_ (Map.elems posts) $ \post -> do
        li_ $ viewShowI_ page_info me_id organization forum board thread post users_map
      -- INPUT FORM AT THE BOTTOM
      -- ACCESS: Thread
      -- * Create: post within a thread
      --
      ebyam m_request mempty $ \request -> do
        permissionsMatchCreateHTML
          threadPackResponsePermissions
          (viewMod TyCreate threadResponseId Nothing request)
          mempty
    PageNumbers.view_ (page_info, routeWith' Home)
  where
  OrganizationPackResponse{..} = organization
  ForumPackResponse{..}        = forum
  BoardPackResponse{..}        = board
  ThreadPackResponse{..}       = thread
  ThreadResponse{..}           = threadPackResponseThread



viewNew
  :: Loader (Maybe ThreadPackResponse)
  -> Maybe ThreadPostRequest
  -> HTMLView_
viewNew l_m_thread m_request = do
  Loader.maybeLoader1 l_m_thread $ \ThreadPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyCreate threadPackResponseThreadId Nothing request



viewEditI
  :: Loader (Maybe ThreadPostPackResponse)
  -> Maybe ThreadPostRequest
  -> HTMLView_
viewEditI l_m_post m_request =
  Loader.maybeLoader1 l_m_post $ \ThreadPostPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (threadPostResponseThreadId threadPostPackResponseThreadPost) (Just threadPostPackResponseThreadPostId) request



viewMod :: TyCRUD -> ThreadId -> Maybe ThreadPostId -> ThreadPostRequest -> HTMLView_
viewMod tycrud thread_id m_post_id request@ThreadPostRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Post"

    li_ $ do
      cldiv_ B.row $ do
        cldiv_ B.colXs2 mempty
        cldiv_ B.colXs9 $ do
          tagsField
            threadPostRequestTags
            (maybe "" id threadPostRequestStateTag)
            (dispatch . ThreadPost.setTag request)
            (dispatch $ ThreadPost.addTag request)
            (dispatch . ThreadPost.deleteTag request)
            (dispatch $ ThreadPost.clearTags request)

          cldiv_ B.well $ do
            p_ $ elemText "bold"
            p_ $ elemText "youtube"
            textarea_ [ className_ B.formControl
                      , "rows" $= "10"
                      , "value" @= body
                      , onChange $ \input -> dispatch $ ThreadPost.setBody request (PostDataRaw $ targetValue input)
                      ] mempty
            -- TODO FIXME: cancel
            button_ [onClick $ \_ _ -> dispatch $ Goto $ routeWith' Home ] $ elemText "cancel"
            button_ [onClick $ \_ _ -> dispatch SaveThreadPost] $ elemText "send"
        cldiv_ B.colXs1 mempty
  where
  body = postDataToBody threadPostRequestBody



viewUserStats :: UserSanitizedPackResponse -> HTMLView_
viewUserStats user =
  div_ $ do
    showBadge "respect "   userSanitizedStatResponseRespect
    showBadge "threads "   userSanitizedStatResponseThreads
    showBadge "posts "     userSanitizedStatResponseThreadPosts
    showBadge "workouts "  userSanitizedStatResponseWorkouts
    -- showBadge' "resources " userSanitizedStatResponseResources,
    -- showBadge' "leurons "   userSanitizedStatResponseLeurons
  where
  UserSanitizedPackResponse{..} = user
  UserSanitizedStatResponse{..} = userSanitizedPackResponseStat



viewPostStats :: ThreadPostStatResponse -> HTMLView_
viewPostStats ThreadPostStatResponse{..} =
  div_ $ do
    showBadge "score: "   $ threadPostStatResponseLikes - threadPostStatResponseDislikes
    showBadge "up: "      threadPostStatResponseLikes
    showBadge "neutral: " threadPostStatResponseNeutral
    showBadge "down: "    threadPostStatResponseDislikes
    showBadge "stars: "   threadPostStatResponseStars
    showBadge "views: "   threadPostStatResponseViews



viewPostData :: PostData -> HTMLView_
viewPostData body =
  case body of
    PostDataEmpty      -> p_ mempty
    PostDataRaw v      -> p_ $ elemText v
    PostDataBBCode v   -> p_ $ elemText v
    -- PostDataBBCode v   ->
    --   case parseBBCode v of
    --        Left err    -> H.p_ [H.text "error: ", H.text err]
    --        Right codes -> H.p_ $ runBBCodeToHTML codes
    PostDataMarkdown v -> p_ $ elemText "markdown"
    _                  -> p_ $ elemText "unknown post body"



postDataToBody :: PostData -> Text
postDataToBody p = case p of
  PostDataRaw v    -> v
  PostDataBBCode v -> v
  _                -> ""
