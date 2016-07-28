{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Boards (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view,
  viewIndex,
  viewIndex_
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
import           LN.UI.ReactFlux.App.Loading          (Loader (..))
import qualified LN.UI.ReactFlux.App.Loading          as Loading
import qualified LN.UI.ReactFlux.App.NotFound         as NotFound (view_)
import qualified LN.UI.ReactFlux.App.Oops             as Oops (view_)
import           LN.UI.ReactFlux.App.PageNumbers      (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers      as PageNumbers
import qualified LN.UI.ReactFlux.App.Route            as Route
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefClasses,
                                                       ahrefClassesName,
                                                       ahrefName, className_,
                                                       classNames_)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Button          (showBadge)
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal        (showTagsSmall)



data Store = Store {
   _lm_organization :: Loader (Maybe OrganizationPackResponse)
 , _lm_forum        :: Loader (Maybe ForumPackResponse)
 , _lm_board        :: Loader (Maybe BoardPackResponse)
 , _l_boards        :: Loader (Map BoardId BoardPackResponse)
 , _l_threads       :: Loader (Map ThreadId ThreadPackResponse)
 , _m_request       :: Maybe BoardRequest
 , _m_requestTag    :: Maybe Text
}



data Action
  = Load
  | Init            OrganizationName ForumName CRUD Params
  | SetRequest      BoardRequest
  | SetRequestState (Maybe BoardRequest) (Maybe Text)
  | Save
  | Edit
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do

    case action of
      Nop                                -> pure st
      Load                               -> action_load
      Init org_sid forum_sid crud params -> action_init org_sid forum_sid crud params
      SetRequest request                 -> action_set_m_request request
      SetRequestState m_req m_tag        -> action_set_m_request_state m_req m_tag
      Save                               -> action_save
      Edit                               -> action_edit

    where
    action_load = pure $ loading st

    action_init org_sid forum_sid crud params = case crud of
      Index -> action_init_index org_sid forum_sid params
      _     -> action_init_crud org_sid forum_sid crud params

    action_init_index org_sid forum_sid params = do
      let
        page_info   = pageInfoFromParams params
        params_list = paramsFromPageInfo page_info
      lr <- runEitherT $ do
        organization  <- mustPassT $ rd $ ApiS.getOrganizationPack' org_sid
        let OrganizationPackResponse{..} = organization
        forum         <- mustPassT $ rd $ ApiS.getForumPack_ByOrganizationId' forum_sid organizationPackResponseOrganizationId
        let ForumPackResponse{..} = forum
        boards        <- mustPassT $ rd $ getBoardPacks_ByForumId' forumPackResponseForumId
        pure (organization, forum, boards)
      rehtie lr (const $ pure $ cantLoad st) $ \(organization, forum, boards) -> do
        pure $ st{
          _lm_organization = Loaded $ Just organization
        , _lm_forum        = Loaded $ Just forum
        , _l_boards        = Loaded $ idmapFrom boardPackResponseBoardId (boardPackResponses boards)
        }

    action_init_crud org_sid forum_sid crud params = case crud of
      ShowS board_sid   -> sync st org_sid forum_sid (Just board_sid)
      New               -> sync st org_sid forum_sid Nothing >>= \st_ -> (pure $ st_{ _m_request = Just defaultBoardRequest })
      EditS board_sid   -> sync st org_sid forum_sid (Just board_sid)
      DeleteS board_sid -> sync st org_sid forum_sid (Just board_sid)
      _                 -> pure st


    action_set_m_request request =
      pure $ st{
        _m_request = Just request
      }

    action_set_m_request_state m_req m_tag = pure $ st{ _m_request = m_req, _m_requestTag = m_tag }

    action_save = do
      case (_m_request, _lm_organization, _lm_forum) of
        (Just board_request, Loaded (Just OrganizationPackResponse{..}), Loaded (Just ForumPackResponse{..})) -> do
          let org_sid = organizationResponseName organizationPackResponseOrganization
          let forum_sid = forumResponseName forumPackResponseForum
          lr <- rd $ postBoard_ByForumId' forumPackResponseForumId board_request
          rehtie lr (const $ pure st) $ \board_response@BoardResponse{..} -> do
            void $ forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForumsBoards org_sid forum_sid (ShowS boardResponseName)) []
            pure st
        _            -> pure st

    action_edit = do
      case (_m_request, _lm_organization, _lm_forum, _lm_board) of
        (Just board_request, Loaded (Just OrganizationPackResponse{..}), Loaded (Just ForumPackResponse{..}), Loaded (Just BoardPackResponse{..})) -> do
          let org_sid    = organizationResponseName organizationPackResponseOrganization
          let forum_sid  = forumResponseName forumPackResponseForum
          let board_sid  = boardResponseName boardPackResponseBoard
          lr <- rd $ putBoard' boardPackResponseBoardId board_request
          rehtie lr (const $ pure st) $ \board_response@BoardResponse{..} -> do
            void $ forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForumsBoards org_sid forum_sid (ShowS board_sid)) []
            pure st
        _           -> pure st



-- | Pull in the latest organization, forum, boards, and recent thread posts
--
sync :: Store -> OrganizationName -> ForumName -> Maybe BoardName -> IO Store
sync st@Store{..} org_sid forum_sid m_board_sid = do
  lr <- runEitherT $ do
    -- Always pull in organization
    --
    organization <- mustPassT $ rd $ ApiS.getOrganizationPack' org_sid
    let OrganizationPackResponse{..} = organization
    forum        <- mustPassT $ rd $ ApiS.getForumPack_ByOrganizationId' forum_sid organizationPackResponseOrganizationId
    let ForumPackResponse{..} = forum
    let ForumResponse{..} = forumPackResponseForum
    x <- ebyam m_board_sid (pure Nothing) $ \board_sid -> do
      -- BoardName exists, so pull in board and threads
      --
      board <- mustPassT $ rd $ ApiS.getBoardPack_ByForumId' board_sid forumResponseId
      threads <- mustPassT $ rd $ getThreadPacks_ByForumId [limitInt forumResponseRecentPostsLimit, WithBoard True, WithThread True, SortOrder SortOrderBy_Dsc, Order OrderBy_CreatedAt] forumResponseId
      pure $ Just (board, threads)
    pure (organization, forum, x)
  rehtie lr (const $ pure st) $ \(organization@OrganizationPackResponse{..}, forum@ForumPackResponse{..}, x) -> do
    pure $ st{
      _m_request       = maybe _m_request (Just . boardResponseToBoardRequest . boardPackResponseBoard) $ fmap sel1 x
    , _lm_organization = Loaded $ Just organization
    , _lm_forum        = Loaded $ Just forum
    , _lm_board        = maybe _lm_board (Loaded . Just) $ fmap sel1 x
    , _l_threads       = maybe _l_threads (Loaded . idmapFrom threadPackResponseThreadId . threadPackResponses) $ fmap sel2 x
    }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
   _lm_organization  = Loaded Nothing
 , _lm_forum         = Loaded Nothing
 , _lm_board         = Loaded Nothing
 , _l_boards         = Loaded Map.empty
 , _l_threads        = Loaded Map.empty
 , _m_request        = Nothing
 , _m_requestTag     = Nothing
}



loading :: Store -> Store
loading st =
  st{
    _lm_organization = Loading
  , _lm_forum        = Loading
  , _lm_board        = Loading
  , _l_boards        = Loading
  , _l_threads       = Loading
  }



cantLoad :: Store -> Store
cantLoad st =
  st{
    _lm_organization = CantLoad
  , _lm_forum        = CantLoad
  , _lm_board        = CantLoad
  , _l_boards        = CantLoad
  , _l_threads       = CantLoad
  }



view_ :: OrganizationName -> ForumName -> CRUD -> HTMLEvent_
view_ org_sid forum_sid crud =
  RF.view view (org_sid,forum_sid,crud) mempty



view :: ReactView (OrganizationName,ForumName,CRUD)
view = defineControllerView "organizations" store $ \st@Store{..} (org_sid,forum_sid,crud) ->
  case crud of
    Index     -> viewIndex st
    ShowS _   -> viewShowS _lm_organization _lm_forum _lm_board _l_threads
    New       -> viewNew _lm_forum _m_requestTag _m_request
    EditS _   -> viewEditS _lm_board _m_requestTag _m_request
    DeleteS _ -> Delete.view_
    _         -> NotFound.view_



viewIndex :: Store -> HTMLView_
viewIndex Store{..} = do
  Loading.loader3 _lm_organization _lm_forum _l_boards $ \m_organization m_forum boards -> do
    case (m_organization, m_forum) of
      (Just organization, Just forum) -> viewIndex_ organization forum boards
      _                               -> mempty



viewIndex_ :: OrganizationPackResponse -> ForumPackResponse -> Map Int64 BoardPackResponse -> HTMLView_
viewIndex_ org_pack@OrganizationPackResponse{..} forum_pack@ForumPackResponse{..} boards_map = do
  h1_ [className_ B.textCenter] $ elemText "Boards"




viewShowS
  :: Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Maybe BoardPackResponse)
  -> Loader (Map ThreadId ThreadPackResponse)
  -> HTMLView_

viewShowS lm_organization lm_forum lm_board l_threads = do
  Loading.loader4 lm_organization lm_forum lm_board l_threads $ \m_organization m_forum m_board threads -> do
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
      p_ [className_ B.lead] $ elemText $ maybe "No description." id boardResponseDescription

      div_ plumbing_threads

  where
  OrganizationResponse{..} = organizationPackResponseOrganization
  ForumResponse{..}        = forumPackResponseForum
  BoardResponse{..}        = boardPackResponseBoard



viewNew
  :: Loader (Maybe ForumPackResponse)
  -> Maybe Text
  -> Maybe BoardRequest
  -> HTMLView_
viewNew lm_forum m_tag m_request = do
  Loading.loader1_ lm_forum $ \ForumPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewNew_ forumPackResponseForumId m_tag request



viewNew_
  :: ForumId
  -> Maybe Text
  -> BoardRequest
  -> HTMLView_
viewNew_ forum_id m_tag request = viewMod TyCreate forum_id Nothing m_tag request



viewEditS
  :: Loader (Maybe BoardPackResponse)
  -> Maybe Text
  -> Maybe BoardRequest
  -> HTMLView_
viewEditS lm_board m_tag m_request =
  Loading.loader1_ lm_board $ \BoardPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (boardResponseOrgId boardPackResponseBoard) (Just boardPackResponseBoardId) m_tag request



viewEditS_
  :: OrganizationId
  -> BoardId
  -> Maybe Text
  -> Maybe BoardRequest
  -> HTMLView_
viewEditS_ organization_id board_id m_tag m_request =
  ebyam m_request mempty $ \request -> viewMod TyUpdate organization_id (Just board_id) m_tag request



viewMod :: TyCRUD -> OrganizationId -> Maybe ForumId -> Maybe Text -> BoardRequest -> HTMLView_
viewMod tycrud organization_id m_forum_id m_tag request@BoardRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Board"



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
