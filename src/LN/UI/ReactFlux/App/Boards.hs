{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Boards (
  viewIndex,
  viewIndex_,
  viewNew,
  viewEditS,
  viewShowS
) where



import           Control.Concurrent                   (forkIO)
import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (void)
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
import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Size
import           LN.T.Thread
import           LN.T.ThreadPost
import           LN.T.User
import LN.UI.Core.App.Board as Board
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
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefClasses,
                                                       ahrefClassesName,
                                                       ahrefName, className_,
                                                       classNames_)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal        (showTagsSmall)




viewIndex
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Map BoardId BoardPackResponse)
  -> HTMLView_

viewIndex page_info l_m_organization l_m_forum l_boards = do
  h1_ [className_ B.textCenter] $ elemText "Boards"
  Loader.maybeLoader1 l_m_organization $ \organization -> do
    Loader.maybeLoader1 l_m_forum $ \forum -> do
      Loader.loader1 l_boards $ \boards -> do
        viewIndex_ organization forum boards



viewIndex_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> Map BoardId BoardPackResponse
  -> HTMLView_

viewIndex_ organization forum boards_map = do
  ul_ [className_ B.listUnstyled] $
    mapM_ (\BoardPackResponse{..} -> do
      let
        BoardResponse{..}     = boardPackResponseBoard
        BoardStatResponse{..} = boardPackResponseStat
        thread                = boardPackResponseLatestThread
        post                  = boardPackResponseLatestThreadPost
        user                  = boardPackResponseLatestThreadPostUser
      li_ $ do
        cldiv_ B.row $ do
          cldiv_ B.colXs1 $ do
            p_ $ elemText "icon"
          cldiv_ B.colXs5 $ do
            cldiv_ B.listGroup $ do
              ahrefClassesName [B.listGroupItem] boardResponseDisplayName $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName (ShowS boardResponseName)
              p_ $ elemText $ maybe "No description." id boardResponseDescription
          cldiv_ B.colXs2 $ do
            showBadge "threads " boardStatResponseThreads
            showBadge "posts "   boardStatResponseThreadPosts
            showBadge "views "   boardStatResponseViews
          cldiv_ B.colXs3 $ do
            case (thread, post, user) of
              (Just ThreadResponse{..}, Just ThreadPostResponse{..}, Just UserSanitizedResponse{..}) -> do
                div_ $ do
                  p_ $ do
                    elemText "Last post by "
                    ahref $ routeWith' (Users (ShowS userSanitizedResponseName))
                  p_ $ do
                    elemText "in "
                    ahref $ routeWith (OrganizationsForumsBoardsThreads organizationResponseName forumResponseName boardResponseName (ShowS threadResponseName)) [(ParamTag_Offset, Offset (-1))]
                  p_ $ elemText $ prettyUTCTimeMaybe threadPostResponseCreatedAt
              _ -> div_ $ p_ $ elemText "No posts."
          cldiv_ B.colXs1 $ do
            cldiv_ B.container $ do
              buttonGroup_VerticalSm1 $ do
                -- ACCESS: Forum
                -- * Create: can create boards
                --
                permissionsMatchCreateHTML
                  forumPackResponsePermissions
                  -- TODO FIXME: Child board
                  (button_newBoard $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName New)
                  mempty

                -- ACCESS: Board
                -- * Update: can edit board settings
                -- * Delete: can delete boads
                --
                permissionsHTML'
                  boardPackResponsePermissions
                  permCreateEmpty
                  permReadEmpty
                  (button_editBoard $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName (EditS boardResponseName))
                  (button_deleteBoard $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName (DeleteS boardResponseName))
                  permExecuteEmpty

    ) $ Map.elems boards_map
  where
  OrganizationPackResponse{..} = organization
  OrganizationResponse{..}     = organizationPackResponseOrganization
  ForumPackResponse{..}        = forum
  ForumResponse{..}            = forumPackResponseForum




viewShowS
  :: Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewShowS l_morganization l_mforum l_mboard l_threads = do
  Loader.loader4 l_morganization l_mforum l_mboard l_threads $ \m_organization m_forum m_board threads -> do
    case (m_organization, m_forum, m_board) of
      (Just organization, Just forum, Just board) ->
        viewShowS_
          organization
          forum
          board
          mempty
      _ -> Oops.view_



viewShowS_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> HTMLView_ -- ^ plumbing threads
  -> HTMLView_

viewShowS_ organization@OrganizationPackResponse{..} forum@ForumPackResponse{..} board@BoardPackResponse{..} plumbing_threads = do
  cldiv_ B.containerFluid $ do
    cldiv_ B.pageHeader $ do
      h2_ $ elemText boardResponseName
      p_ [className_ B.lead] $ elemText $ maybe "No description." id boardResponseDescription
      buttonGroup_HorizontalSm1 $ do
        -- ACCESS: Board
        -- * Create: can create threads and sub-boards
        -- * Update: can edit board settings
        -- * Delete: can delete board
        --
        permissionsHTML'
          boardPackResponsePermissions
          (button_newThread $ routeWith' $ OrganizationsForumsBoardsThreads organizationResponseName forumResponseName boardResponseName New)
          permReadEmpty
          (button_editBoard $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName (EditS boardResponseName))
          (button_deleteBoard $ routeWith' $ OrganizationsForumsBoards organizationResponseName forumResponseName (DeleteS boardResponseName))
          permExecuteEmpty
    div_ plumbing_threads

  where
  OrganizationResponse{..} = organizationPackResponseOrganization
  ForumResponse{..}        = forumPackResponseForum
  BoardResponse{..}        = boardPackResponseBoard



viewNew
  :: Loader (Maybe ForumPackResponse)
  -> Maybe BoardRequest
  -> HTMLView_
viewNew l_mforum m_request = do
  Loader.maybeLoader1 l_mforum $ \ForumPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyCreate forumPackResponseForumId Nothing request



viewEditS
  :: Loader (Maybe BoardPackResponse)
  -> Maybe BoardRequest
  -> HTMLView_
viewEditS l_mboard m_request =
  Loader.maybeLoader1 l_mboard $ \BoardPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (boardResponseOrgId boardPackResponseBoard) (Just boardPackResponseBoardId) request



viewMod :: TyCRUD -> OrganizationId -> Maybe ForumId -> BoardRequest -> HTMLView_
viewMod tycrud organization_id m_forum_id request@BoardRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Board"

    mandatoryNameField boardRequestDisplayName (dispatch . Board.setDisplayName request)

    optionalDescriptionField boardRequestDescription
      (dispatch . Board.setDescription request)
      (dispatch $ Board.clearDescription request)

    mandatoryBooleanYesNoField "Anonymous" boardRequestIsAnonymous False
      (dispatch . Board.setIsAnonymous request)

    mandatoryBooleanYesNoField "Can create sub-boards" boardRequestCanCreateSubBoards True
      (dispatch . Board.setCanCreateSubBoards request)

    mandatoryBooleanYesNoField "Can create threads" boardRequestCanCreateThreads True
      (dispatch . Board.setCanCreateThreads request)

    suggestedTagsField
      boardRequestSuggestedTags
      (maybe "" id boardRequestStateSuggestedTag)
      (dispatch . Board.setSuggestedTag request)
      (dispatch $ Board.addSuggestedTag request)
      (dispatch . Board.deleteSuggestedTag request)
      (dispatch $ Board.clearSuggestedTags request)

    tagsField
      boardRequestTags
      (maybe "" id boardRequestStateTag)
      (dispatch . Board.setTag request)
      (dispatch $ Board.addTag request)
      (dispatch . Board.deleteTag request)
      (dispatch $ Board.clearTags request)

    createButtonsCreateEditCancel
      m_forum_id
      (dispatch Save)
      (const $ dispatch Save)
      (routeWith' Home)
