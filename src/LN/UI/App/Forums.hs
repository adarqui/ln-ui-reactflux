{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase #-}

module LN.UI.App.Forums (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view,
  viewIndex,
  viewIndex_
) where



import Control.Monad (void)
import           Control.Concurrent              (forkIO)
import           Control.DeepSeq                 (NFData)
import           Control.Monad.Trans.Either      (EitherT, runEitherT)
import           Data.Ebyam                      (ebyam)
import           Data.Int                        (Int64)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import           Data.Rehtie                     (rehtie)
import           Data.Text                       (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Haskell.Helpers.Either          (mustPassT)
import           React.Flux                      hiding (view)
import qualified React.Flux                      as RF
import qualified Web.Bootstrap3                  as B

import           LN.Api
import qualified LN.Api.String                   as ApiS
import           LN.Generate.Default             (defaultForumRequest)
import           LN.T.Convert                    (forumResponseToForumRequest)
import           LN.T.Forum
import           LN.T.Organization
import           LN.T.Pack.Forum
import           LN.T.Pack.Organization
import LN.T.Pack.Board
import           LN.T.Size                       (Size (..))
import           LN.T.User                       (UserSanitizedResponse (..))
import           LN.UI.Access
import qualified LN.UI.App.Delete                as Delete
import qualified LN.UI.App.Gravatar              as Gravatar
import           LN.UI.App.Loading               (Loader (..))
import qualified LN.UI.App.Loading               as Loading
import           LN.UI.App.PageNumbers           (runPageInfo)
import qualified LN.UI.App.PageNumbers           as PageNumbers
import qualified LN.UI.App.NotFound as NotFound (view_)
import qualified LN.UI.App.Route                 as Route
import           LN.UI.Helpers.DataList          (deleteNth)
import           LN.UI.Helpers.DataText          (tshow)
import           LN.UI.Helpers.DataTime          (prettyUTCTimeMaybe)
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref, ahrefClasses,
                                                  ahrefClassesName, ahrefName,
                                                  className_, classNames_)
import           LN.UI.Router                    (CRUD (..), Params, Route (..),
                                                  RouteWith (..), TyCRUD (..),
                                                  emptyParams, linkName,
                                                  routeWith, routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)
import           LN.UI.Types
import           LN.UI.View.Button
import           LN.UI.View.Button               (showBadge)
import           LN.UI.View.Field
import           LN.UI.View.Internal             (showTagsSmall)



data Store = Store {
  _lm_forums      :: Loader (Map ForumId ForumPackResponse),
  _lm_organization :: Loader (Maybe OrganizationPackResponse),
  _lm_forum        :: Loader (Maybe ForumPackResponse),
  _l_boards        :: Loader (Map BoardId BoardPackResponse),
  _m_request       :: Maybe ForumRequest,
  _m_requestTag    :: Maybe Text
}



data Action
  = Load
  | Init            OrganizationName CRUD Params
  | SetRequest      ForumRequest
  | SetRequestState (Maybe ForumRequest) (Maybe Text)
  | Save
  | Edit
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do

    case action of
      Nop                         -> pure st
      Load                        -> action_load
      Init org_sid crud params    -> action_init org_sid crud params
      SetRequest request          -> action_set_m_request request
      SetRequestState m_req m_tag -> action_set_m_request_state m_req m_tag
      Save                        -> action_save
      Edit                        -> action_edit

    where
    action_load = do
      pure $ st{
        _lm_forums       = Loading,
        _lm_organization = Loading,
        _lm_forum        = Loading,
        _l_boards        = Loading
      }

    action_init org_sid crud params = case crud of
      Index -> action_init_index org_sid params
      _     -> action_init_crud org_sid crud params

    action_init_index org_sid params = do
      let
        page_info   = pageInfoFromParams params
        params_list = paramsFromPageInfo page_info
      lr <- runEitherT $ do
        organization@OrganizationPackResponse{..}  <- mustPassT $ rd $ ApiS.getOrganizationPack' org_sid
        forums        <- mustPassT $ rd $ getForumPacks_ByOrganizationId' organizationPackResponseOrganizationId
        pure (organization, forums)
      rehtie lr (const $ pure st) $ \(organization, forums) -> do
        pure $ st{
          _lm_organization = Loaded $ Just organization
        , _lm_forums       = Loaded $ idmapFrom forumPackResponseForumId (forumPackResponses forums)
        }

    action_init_crud org_sid crud params = case crud of
      ShowS forum_sid   -> sync st org_sid forum_sid
      New               -> pure $ st{ _m_request = Just defaultForumRequest }
      EditS forum_sid   -> sync st org_sid forum_sid
      DeleteS forum_sid -> sync st org_sid forum_sid
      _                 -> pure st


    action_set_m_request request =
      pure $ st{
        _m_request = Just request
      }

    action_set_m_request_state m_req m_tag = pure $ st{ _m_request = m_req, _m_requestTag = m_tag }

    action_save = do
      case (_m_request, _lm_organization) of
        (Just forum_request, Loaded (Just OrganizationPackResponse{..})) -> do
          let org_sid = organizationResponseName organizationPackResponseOrganization
          lr <- rd $ postForum_ByOrganizationId' organizationPackResponseOrganizationId forum_request
          rehtie lr (const $ pure st) $ \forum_response@ForumResponse{..} -> do
            void $ forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForums org_sid (ShowS forumResponseName)) []
            pure st
        _            -> pure st

    action_edit = do
      case (_m_request, _lm_organization, _lm_forum) of
        (Just forum_request, Loaded (Just OrganizationPackResponse{..}), Loaded (Just ForumPackResponse{..})) -> do
          let org_sid =organizationResponseName organizationPackResponseOrganization
          lr <- rd $ putForum' forumPackResponseForumId $ forum_request
          rehtie lr (const $ pure st) $ \forum_response@ForumResponse{..} -> do
            void $ forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForums org_sid (ShowS forumResponseName)) []
            pure st
        _           -> pure st



sync :: Store -> Text -> Text -> IO Store
sync st@Store{..} org_sid forum_sid = do
  lr <- runEitherT $ do
    organization@OrganizationPackResponse{..} <- mustPassT $ rd $ ApiS.getOrganizationPack' org_sid
    forum@ForumPackResponse{..}        <- mustPassT $ rd $ ApiS.getForumPack_ByOrganizationId' forum_sid organizationPackResponseOrganizationId
    boards <- mustPassT $ rd $ getBoardPacks_ByForumId' forumPackResponseForumId
    pure (organization, forum, boards)
  rehtie lr (const $ pure st) $ \(organization@OrganizationPackResponse{..}, forum@ForumPackResponse{..}, boards@BoardPackResponses{..}) -> do
    pure $ st{
      _m_request      = Just $ forumResponseToForumRequest forumPackResponseForum
    , _lm_organization = Loaded $ Just organization
    , _lm_forum        = Loaded $ Just forum
    , _l_boards        = Loaded $ idmapFrom boardPackResponseBoardId boardPackResponses
    }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _lm_organization  = Loaded Nothing,
  _lm_forums        = Loaded Map.empty,
  _lm_forum         = Loaded Nothing,
  _l_boards         = Loaded Map.empty,
  _m_request        = Nothing,
  _m_requestTag     = Nothing
}



view_ :: OrganizationName -> CRUD -> HTMLEvent_
view_ org_sid crud =
  RF.view view (org_sid,crud) mempty



view :: ReactView (OrganizationName,CRUD)
view = defineControllerView "organizations" store $ \st@Store{..} (org_sid,crud) ->
  case crud of
    Index     -> viewIndex st
    ShowS _   -> viewShowS _lm_organization _lm_forum _l_boards
    New       -> viewNew _lm_organization _m_requestTag _m_request
    EditS _   -> viewEditS  _lm_forum _m_requestTag _m_request
    DeleteS _ -> Delete.view_
    _         -> NotFound.view_



viewIndex :: Store -> HTMLView_
viewIndex Store{..} = do
  Loading.loader2 _lm_organization _lm_forums $ \m_organization forums -> do
    case m_organization of
      Nothing           -> mempty
      Just organization -> viewIndex_ organization forums



viewIndex_ :: OrganizationPackResponse -> Map Int64 ForumPackResponse -> HTMLView_
viewIndex_ org_pack@OrganizationPackResponse{..} forums_map = do
  h1_ [className_ B.textCenter] $ elemText "Forums"

  -- ACCESS: Organization
  -- * Create: can create forums
  --
  permissionsMatchCreateHTML
    organizationPackResponsePermissions
    (button_newForum $ routeWith' $ OrganizationsForums organizationResponseName New)
    mempty

  cldiv_ B.listUnstyled $
    mapM_ (\ForumPackResponse{..} -> do
      let
        ForumResponse{..}     = forumPackResponseForum
        ForumStatResponse{..} = forumPackResponseStat
      li_ $ do
        cldiv_ B.row $ do
          cldiv_ B.colXs1 $ p_ $ elemText "icon"
        cldiv_ B.colXs6 $ do
          cldiv_ B.listGroup $ do
            ahrefClassesName [B.listGroupItem] forumResponseDisplayName $ routeWith' $ OrganizationsForums organizationResponseName (ShowS forumResponseName)
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
    ) $ Map.elems forums_map

  where
  org@OrganizationResponse{..} = organizationPackResponseOrganization




viewShowS
  :: Loader (Maybe OrganizationPackResponse)
  -> Loader (Maybe ForumPackResponse)
  -> Loader (Map BoardId BoardPackResponse)
  -> HTMLView_

viewShowS lm_organization lm_forum l_boards = do
  Loading.loader2_ lm_organization lm_forum $ \organization@OrganizationPackResponse{..} forum@ForumPackResponse{..} -> do
    mempty



viewNew :: Loader (Maybe OrganizationPackResponse) -> Maybe Text -> Maybe ForumRequest -> HTMLView_
viewNew l_organization m_tag m_request =
  Loading.loader1_ l_organization $ \OrganizationPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewNew_ organizationPackResponseOrganizationId m_tag request



viewNew_ :: OrganizationId -> Maybe Text -> ForumRequest -> HTMLView_
viewNew_ organization_id m_tag request = viewMod TyCreate organization_id Nothing m_tag request



viewEditS :: Loader (Maybe ForumPackResponse) -> Maybe Text -> Maybe ForumRequest -> HTMLView_
viewEditS lm_forum m_tag m_request =
  Loading.loader1_ lm_forum $ \ForumPackResponse{..} ->
    ebyam m_request mempty $ \request -> viewMod TyUpdate (forumResponseOrgId forumPackResponseForum) (Just forumPackResponseForumId) m_tag request



viewEditS_ :: OrganizationId -> ForumId -> Maybe Text -> Maybe ForumRequest -> HTMLView_
viewEditS_ organization_id forum_id m_tag m_request =
  ebyam m_request mempty $ \request -> viewMod TyUpdate organization_id (Just forum_id) m_tag request



viewMod :: TyCRUD -> OrganizationId -> Maybe ForumId -> Maybe Text -> ForumRequest -> HTMLView_
viewMod tycrud organization_id m_forum_id m_tag request@ForumRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Forum"

    mandatoryNameField forumRequestDisplayName
      (\input -> dispatch $ SetRequest $ request{forumRequestDisplayName = input})

    optionalDescriptionField forumRequestDescription
      (\input -> dispatch $ SetRequest $ request{forumRequestDescription = Just input})
      (dispatch $ SetRequest $ request{forumRequestDescription = Nothing})

    mandatoryIntegerField "Threads per Board" forumRequestThreadsPerBoard 20 10 50 10
      (\input -> dispatch $ SetRequest $ request{forumRequestThreadsPerBoard = input})

    mandatoryIntegerField "Posts per Thread" forumRequestThreadPostsPerThread 20 10 50 10
      (\input -> dispatch $ SetRequest $ request{forumRequestThreadPostsPerThread = input})

    mandatoryIntegerField "Recent threads (limit)" forumRequestRecentThreadsLimit 10 0 20 1
      (\input -> dispatch $ SetRequest $ request{forumRequestRecentThreadsLimit = input})

    mandatoryIntegerField "Recent posts (limit)" forumRequestRecentPostsLimit 10 0 20 1
      (\input -> dispatch $ SetRequest $ request{forumRequestRecentPostsLimit = input})

    mandatoryIntegerField "Messages of the week (limit)" forumRequestMotwLimit 10 0 20 1
      (\input -> dispatch $ SetRequest $ request{forumRequestMotwLimit = input})

    mandatoryVisibilityField forumRequestVisibility
      (\input -> dispatch $ SetRequest $ request{forumRequestVisibility = input})

    tagsField
      forumRequestTags
      (maybe "" id m_tag)
      (\input -> dispatch $ SetRequestState (Just request) (Just input))
      (dispatch $ SetRequestState (Just $ request{forumRequestTags = maybe forumRequestTags (\tag -> forumRequestTags <> [tag]) m_tag}) Nothing)
      (\idx -> dispatch $ SetRequest $ request{forumRequestTags = deleteNth idx forumRequestTags})
      (dispatch $ SetRequest $ request{forumRequestTags = []})

    createButtonsCreateEditCancel
      m_forum_id
      (dispatch Save)
      (const $ dispatch Edit)
      (routeWith' Home)


dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
