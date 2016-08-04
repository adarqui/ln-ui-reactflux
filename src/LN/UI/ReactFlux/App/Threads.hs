{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Threads (
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
import qualified LN.UI.Core.App.Thread as Thread
import LN.UI.ReactFlux.App.Core.Shared
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
import           LN.UI.ReactFlux.View.Button          (showBadge)
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal        (showTagsSmall)




viewIndex
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewIndex page_info l_m_organization l_m_forum l_m_board l_threads = do
  h1_ [className_ B.textCenter] $ elemText "Threads"
  Loader.maybeLoader1 l_m_organization $ \organization -> do
    Loader.maybeLoader1 l_m_forum $ \forum -> do
      Loader.maybeLoader1 l_m_board $ \board -> do
        Loader.loader1 l_threads $ \threads -> do
          viewIndex_ organization forum board threads



viewIndex_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> Map ThreadId ThreadPackResponse
  -> HTMLView_

viewIndex_ organization forum board threads = do
  p_ $ elemText "..."



viewShowS
  :: Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Map ThreadId ThreadPostPackResponse)
  -> HTMLView_

viewShowS l_m_organization l_m_forum l_m_board l_m_thread l_posts = do
  Loader.loader5 l_m_organization l_m_forum l_m_board l_m_thread l_posts $ \m_organization m_forum m_board m_thread posts -> do
    case (m_organization, m_forum, m_board, m_thread) of
      (Just organization, Just forum, Just board, Just thread) ->
        viewShowS_
          organization
          forum
          board
          thread
          mempty
      _ -> Oops.view_



viewShowS_
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> HTMLView_ -- ^ plumbing thread posts
  -> HTMLView_

viewShowS_ organization@OrganizationPackResponse{..} forum@ForumPackResponse{..} board@BoardPackResponse{..} thread@ThreadPackResponse{..} plumbing_posts = do
  cldiv_ B.containerFluid $ do
    cldiv_ B.pageHeader $ do
      h2_ $ elemText threadResponseName
      p_ [className_ B.lead] $ elemText $ maybe "No description." id boardResponseDescription
      div_ plumbing_posts

  where
  OrganizationResponse{..} = organizationPackResponseOrganization
  ForumResponse{..}        = forumPackResponseForum
  BoardResponse{..}        = boardPackResponseBoard
  ThreadResponse{..}       = threadPackResponseThread



viewNew
  :: Loader (Maybe BoardPackResponse)
  -> Maybe ThreadRequest
  -> HTMLView_
viewNew l_m_board m_request = do
  Loader.maybeLoader1 l_m_board $ \BoardPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyCreate boardPackResponseBoardId Nothing request



viewEditS
  :: Loader (Maybe ThreadPackResponse)
  -> Maybe ThreadRequest
  -> HTMLView_
viewEditS l_m_thread m_request =
  Loader.maybeLoader1 l_m_thread $ \ThreadPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (threadResponseBoardId threadPackResponseThread) (Just threadPackResponseThreadId) request



viewMod :: TyCRUD -> BoardId -> Maybe ThreadId -> ThreadRequest -> HTMLView_
viewMod tycrud boardid m_thread_id request@ThreadRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Thread"

    mandatoryNameField threadRequestDisplayName (dispatch . Thread.setDisplayName request)

    optionalDescriptionField threadRequestDescription
      (dispatch . Thread.setDescription request)
      (dispatch $ Thread.clearDescription request)

    mandatoryBooleanYesNoField "Sticky" threadRequestSticky False
      (dispatch . Thread.setSticky request)

    mandatoryBooleanYesNoField "Locked" threadRequestLocked False
      (dispatch . Thread.setLocked request)

    p_ $ elemText "poll: TODO"

    tagsField
      threadRequestTags
      (maybe "" id threadRequestStateTag)
      (dispatch . Thread.setTag request)
      (dispatch $ Thread.addTag request)
      (dispatch . Thread.deleteTag request)
      (dispatch $ Thread.clearTags request)

    createButtonsCreateEditCancel
      m_thread_id
      (dispatch Save)
      (const $ dispatch Save)
      (routeWith' Home)
