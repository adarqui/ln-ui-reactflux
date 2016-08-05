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
import           Control.Monad                        (void, forM_)
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
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Map ThreadPostId ThreadPostPackResponse)
  -> HTMLView_

viewIndex page_info l_m_organization l_m_forum l_m_board l_m_thread l_posts = do
  h1_ [className_ B.textCenter] $ elemText "Posts"
  Loader.maybeLoader1 l_m_organization $ \organization -> do
    Loader.maybeLoader1 l_m_forum $ \forum -> do
      Loader.maybeLoader1 l_m_board $ \board -> do
        Loader.maybeLoader1 l_m_thread $ \thread -> do
          Loader.loader1 l_posts $ \posts -> do
            viewIndex_ page_info organization forum board thread posts



viewIndex_
  :: PageInfo
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> Map ThreadPostId ThreadPostPackResponse
  -> HTMLView_

viewIndex_ page_info organization forum board thread posts = do
  p_ $ elemText "..."



viewShowI
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Maybe ThreadPackResponse)
  -> Loader (Maybe ThreadPostPackResponse)
  -> HTMLView_

viewShowI page_info l_m_organization l_m_forum l_m_board l_m_thread l_m_post = do
  Loader.maybeLoader5 l_m_organization l_m_forum l_m_board l_m_thread l_m_post $ \organization forum board thread post -> do
    viewShowI_
      page_info
      organization
      forum
      board
      thread
      post
      mempty



viewShowI_
  :: PageInfo
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> ThreadPostPackResponse
  -> HTMLView_ -- ^ plumbing?
  -> HTMLView_

viewShowI_ page_info organization@OrganizationPackResponse{..} forum@ForumPackResponse{..} board@BoardPackResponse{..} thread@ThreadPackResponse{..} post@ThreadPostPackResponse{..} plumbing_threads = do
  cldiv_ B.containerFluid $ do
    cldiv_ B.pageHeader $ do
      p_ $ elemText "post"
      div_ plumbing_threads

  where
  OrganizationResponse{..} = organizationPackResponseOrganization
  ForumResponse{..}        = forumPackResponseForum
  BoardResponse{..}        = boardPackResponseBoard
  ThreadResponse{..}       = threadPackResponseThread
  ThreadPostResponse{..}   = threadPostPackResponseThreadPost



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
