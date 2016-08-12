{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Forums (
  viewIndex,
  viewIndex_,
  viewNew,
  viewEditS,
  viewShowS,
  viewMessagesOfTheWeek_,
  viewRecentPosts_
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, void)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Ebyam                            (ebyam)
import           Data.Int                              (Int64)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import           Data.Tuple.Select
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Haskell.Helpers.Either                (mustPassT)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.Api
import qualified LN.Api.String                         as ApiS
import           LN.Generate.Default                   (defaultForumRequest)
import           LN.T.Board
import           LN.T.Convert                          (forumResponseToForumRequest)
import           LN.T.Forum
import           LN.T.Organization
import           LN.T.Pack.Board
import           LN.T.Pack.Forum
import           LN.T.Pack.Organization
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import qualified LN.UI.Core.App.Forum                  as Forum
import           LN.UI.Core.Helpers.DataList           (deleteNth)
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.HaskellApiHelpers  (rd)
import           LN.UI.Core.Helpers.Map                (idmapFrom)
import           LN.UI.Core.PageInfo                   (PageInfo (..),
                                                        defaultPageInfo,
                                                        pageInfoFromParams,
                                                        paramsFromPageInfo)
import           LN.UI.Core.Router                     (CRUD (..), Params,
                                                        Route (..),
                                                        RouteWith (..),
                                                        TyCRUD (..),
                                                        emptyParams, linkName,
                                                        routeWith, routeWith')
import           LN.UI.Core.Sort
import           LN.UI.ReactFlux.Access
import qualified LN.UI.ReactFlux.App.Boards            as Boards
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete            as Delete
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loading
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound (view_)
import qualified LN.UI.ReactFlux.App.Oops              as Oops (view_)
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefClasses,
                                                        ahrefClassesName,
                                                        ahrefName, className_,
                                                        classNames_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Button           (showBadge)
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal         (showTagsSmall)




viewIndex
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Map ForumId ForumPackResponse)
  -> HTMLView_

viewIndex !page_info l_m_organization l_forums = do
  defineViewWithSKey "forums-index-1" (page_info, l_m_organization, l_forums) $ \(page_info, l_m_organization, l_forums) -> do
    h1_ [className_ B.textCenter] $ elemText "Forums"
    Loading.loader2 l_m_organization l_forums $ \m_organization forums -> do
      case m_organization of
        Nothing           -> mempty
        Just organization -> viewIndex_ page_info organization forums



viewIndex_
  :: PageInfo
  -> OrganizationPackResponse
  -> Map Int64 ForumPackResponse
  -> HTMLView_

viewIndex_ !page_info !organization !forums_map = do
  defineViewWithSKey "forums-index-2" (page_info, organization, forums_map) $ \(page_info, organization, forums_map) -> do

    -- ACCESS: Organization
    -- * Create: can create forums
    --
    permissionsMatchCreateHTML
      organizationPackResponsePermissions
      (button_newForum $ routeWith' $ OrganizationsForums organizationResponseName New)
      mempty

    cldiv_ B.listUnstyled $
      forM_ (Map.elems forums_map) $ \ForumPackResponse{..} -> do
        let
          ForumResponse{..}     = forumPackResponseForum
          ForumStatResponse{..} = forumPackResponseStat
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs1 $ p_ $ elemText "icon"
          cldiv_ B.colXs6 $ do
              p_ $ ahrefName forumResponseDisplayName $ routeWith' $ OrganizationsForums organizationResponseName (ShowS forumResponseName)
              p_ $ elemText $ maybe "No description." id forumResponseDescription
              showTagsSmall forumResponseTags
          cldiv_ B.colXs2 $ do
            showBadge "boards "  forumStatResponseBoards
            showBadge "threads " forumStatResponseThreads
            showBadge "posts "   forumStatResponseThreadPosts
            showBadge "views "   forumStatResponseViews
          cldiv_ B.colXs2 $ p_ $ elemText "created-at"
          cldiv_ B.colXs1 $ do

            -- ACCESS: Forum
            -- * Update: can edit forum settings
            -- * Delete: can delete a forum
            --
            permissionsHTML'
              forumPackResponsePermissions
              permCreateEmpty
              permReadEmpty
              (button_editForum $ routeWith' $ OrganizationsForums organizationResponseName (EditS forumResponseName))
              (button_deleteForum $ routeWith' $ OrganizationsForums organizationResponseName (DeleteS forumResponseName))
              permExecuteEmpty

    where
    OrganizationPackResponse{..} = organization
    org@OrganizationResponse{..} = organizationPackResponseOrganization




viewShowS
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Map BoardId BoardPackResponse)
  -> Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewShowS !page_info !l_m_organization !l_m_forum !l_boards !l_recent_posts = do
  defineViewWithSKey "forums-show-1" (l_m_organization, l_m_forum, l_boards, l_recent_posts) $ \(l_m_organization, l_m_forum, l_boards, l_recent_posts) -> do
    Loading.loader4 l_m_organization l_m_forum l_boards l_recent_posts $ \m_organization m_forum boards recent_posts -> do
      case (m_organization, m_forum) of
        (Just organization, Just forum) ->
          viewShowS_
            page_info
            organization
            forum
            (Boards.viewIndex_ page_info organization forum boards)
            (viewRecentPosts_ organization forum recent_posts)
            (viewMessagesOfTheWeek_ organization forum)
        _ -> Oops.view_



viewShowS_
  :: PageInfo
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> HTMLView_ -- ^ plumbing boards
  -> HTMLView_ -- ^ plumbing recent posts
  -> HTMLView_ -- ^ plumbing messages of the week
  -> HTMLView_

viewShowS_ !page_info !organization !forum plumbing_boards !plumbing_recent_posts !plumbing_messages_of_the_week = do
  defineViewWithSKey "forums-show-2" (page_info, organization, forum, plumbing_boards, plumbing_recent_posts, plumbing_messages_of_the_week) $ \(page_info, organization, forum, plumbing_boards, plumbing_recent_posts, plumbing_messages_of_the_week) -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h2_ $ elemText forumResponseName
        p_ [className_ B.lead] $ elemText $ maybe "No description." id forumResponseDescription

        -- ACCESS: Forum
        -- * Create: can create boards within a forum
        -- * Update: can edit forum settings
        -- * Delete: can delete the forum
        --
        buttonGroup_HorizontalSm1 $ do
          permissionsHTML'
            forumPackResponsePermissions
            (button_newBoard $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName New)
            permReadEmpty
            (button_editForum $ routeWith' $ OrganizationsForums organizationResponseName (EditS forumResponseName))
            (button_deleteForum $ routeWith' $ OrganizationsForums organizationResponseName (DeleteS forumResponseName))
            permExecuteEmpty

        div_ plumbing_boards
        div_ plumbing_recent_posts
        div_ plumbing_messages_of_the_week

    where
    OrganizationPackResponse{..} = organization
    OrganizationResponse{..}     = organizationPackResponseOrganization
    ForumPackResponse{..}        = forum
    ForumResponse{..}            = forumPackResponseForum




viewNew
  :: Loader (Maybe OrganizationPackResponse)
  -> Maybe ForumRequest
  -> HTMLView_

viewNew !l_m_organization !m_request = do
  Loading.maybeLoader1 l_m_organization $ \OrganizationPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyCreate organizationPackResponseOrganizationId Nothing request



viewEditS
  :: Loader (Maybe ForumPackResponse)
  -> Maybe ForumRequest
  -> HTMLView_

viewEditS !l_m_forum !m_request =
  Loading.maybeLoader1 l_m_forum $ \ForumPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (forumResponseOrgId forumPackResponseForum) (Just forumPackResponseForumId) request



viewMod
  :: TyCRUD
  -> OrganizationId
  -> Maybe ForumId
  -> ForumRequest
  -> HTMLView_

viewMod !tycrud !organization_id !m_forum_id !request = do
  defineViewWithSKey "forums-mod" (tycrud, organization_id, m_forum_id, request) $ \(tycrud, organization_id, m_forum_id, request) -> do
    div_ $ do
      h1_ $ elemText $ linkName tycrud <> " Forum"

      mandatoryNameField forumRequestDisplayName (dispatch . Forum.setDisplayName request)

      optionalDescriptionField forumRequestDescription
        (dispatch . Forum.setDescription request)
        (dispatch $ Forum.clearDescription request)

      mandatoryIntegerField "Threads per Board" forumRequestThreadsPerBoard 20 10 50 10
        (dispatch . Forum.setThreadsPerBoard request)

      mandatoryIntegerField "Posts per Thread" forumRequestThreadPostsPerThread 20 10 50 10
        (dispatch . Forum.setThreadPostsPerThread request)

      mandatoryIntegerField "Recent threads (limit)" forumRequestRecentThreadsLimit 10 0 20 1
        (dispatch . Forum.setRecentThreadsLimit request)

      mandatoryIntegerField "Recent posts (limit)" forumRequestRecentPostsLimit 10 0 20 1
        (dispatch . Forum.setRecentPostsLimit request)

      mandatoryIntegerField "Messages of the week (limit)" forumRequestMotwLimit 10 0 20 1
        (dispatch . Forum.setMotwLimit request)

      mandatoryVisibilityField forumRequestVisibility
        (dispatch . Forum.setVisibility request)

      tagsField
        forumRequestTags
        (maybe "" id forumRequestStateTag)
        (dispatch . Forum.setTag request)
        (dispatch $ Forum.addTag request)
        (dispatch . Forum.deleteTag request)
        (dispatch $ Forum.clearTags request)

      createButtonsCreateEditCancel
        m_forum_id
        (dispatch Save)
        (const $ dispatch Save)
        (routeWith' Home)
      where
      ForumRequest{..} = request



--
-- Re: ADARQ's Journal by adarqui (Progress Journals & Experimental Routines) Today at 06:00:30 pm
--
viewMessagesOfTheWeek_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> HTMLView_

viewMessagesOfTheWeek_ !organization !forum = do
  defineViewWithSKey "forums-messages-of-the-week" (organization, forum) $ \(organization, forum) -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h4_ $ elemText "Messages of the week"



--
-- Re: ADARQ's Journal by adarqui (Progress Journals & Experimental Routines) Today at 06:00:30 pm
--
viewRecentPosts_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> Map ThreadPostId ThreadPostPackResponse
  -> HTMLView_

viewRecentPosts_ !organization !forum !posts_map = do
  defineViewWithSKey "forums-recent-posts" (organization, forum, posts_map) $ \(organization, forum, posts_map) -> do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ h4_ $ elemText "Recent posts"
      ul_ [className_ B.listUnstyled] $ do
        forM_ (sortThreadPostPacks SortOrderBy_Dsc posts_map) $ \pack@ThreadPostPackResponse{..} -> do
          let
            post@ThreadPostResponse{..} = threadPostPackResponseThreadPost
            m_board = threadPostPackResponseWithBoard
            m_thread = threadPostPackResponseWithThread
            board_name = maybe "unknown" boardResponseName m_board
            thread_name = maybe "unknown" threadResponseName m_thread
            user@UserSanitizedResponse{..} = threadPostPackResponseUser
          li_ $ do
            p_ $ do
              ahrefName (thread_name <> "/" <> tshow threadPostResponseId) $ routeWith' (OrganizationsForumsBoardsThreadsPosts organizationResponseName forumResponseName board_name thread_name (ShowI threadPostResponseId))
              elemText " by "
              ahref $ routeWith' (Users (ShowS userSanitizedResponseName))
              elemText " at "
              elemText $ tshow threadPostResponseCreatedAt

    where
    OrganizationPackResponse{..} = organization
    OrganizationResponse{..}     = organizationPackResponseOrganization
    ForumPackResponse{..}        = forum
    ForumResponse{..}            = forumPackResponseForum
