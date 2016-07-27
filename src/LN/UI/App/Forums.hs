{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Forums (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view
) where



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

import           LN.Api                          (getForumPack', getForumPacks_ByOrganizationId',
                                                  postForum_ByOrganizationId',
                                                  putForum')
import           LN.Api.String                   (getOrganizationPack')
import qualified LN.Api.String                   as ApiS (getForumPack_ByOrganizationId',
                                                          getOrganization',
                                                          getOrganizationPack')
import           LN.Generate.Default             (defaultForumRequest)
import           LN.T.Convert                    (forumResponseToForumRequest)
import           LN.T.Forum
import           LN.T.Organization
import           LN.T.Pack.Forum
import           LN.T.Pack.Organization
import           LN.T.Size                       (Size (..))
import           LN.T.User                       (UserSanitizedResponse (..))
import           LN.UI.Access
import qualified LN.UI.App.Delete                as Delete
import qualified LN.UI.App.Gravatar              as Gravatar
import           LN.UI.App.Loading               (Loader (..))
import qualified LN.UI.App.Loading               as Loading
import           LN.UI.App.PageNumbers           (runPageInfo)
import qualified LN.UI.App.PageNumbers           as PageNumbers
import qualified LN.UI.App.Route                 as Route
import           LN.UI.Helpers.DataList          (deleteNth)
import           LN.UI.Helpers.DataText          (tshow)
import           LN.UI.Helpers.DataTime          (prettyUTCTimeMaybe)
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref, ahrefName, className_, ahrefClasses,
                                                  classNames_)
import           LN.UI.Router                    (CRUD (..), Params, Route (..),
                                                  RouteWith (..), TyCRUD (..),
                                                  emptyParams, linkName,
                                                  routeWith, routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)
import           LN.UI.Types                     (HTMLEvent_, HTMLView_,
                                                  OrganizationName)
import           LN.UI.View.Button
import           LN.UI.View.Field
import           LN.UI.View.Internal             (showTagsSmall)



data Store = Store {
  _l_lm_forums     :: Loader (Map Int64 ForumPackResponse),
  _lm_organization :: Loader (Maybe OrganizationPackResponse),
  _lm_forum        :: Loader (Maybe ForumPackResponse),
  _m_request       :: Maybe ForumRequest,
  _m_requestTag    :: Maybe Text
}



data Action
  = Load
  | Init            OrganizationName CRUD Params
  | SetRequest      ForumRequest
  | SetRequestState (Maybe ForumRequest) (Maybe Text)
  | Save
  | Edit            Int64
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
      Edit edit_id                -> action_edit edit_id

    where
    action_load = do
      pure $ st{
        _l_lm_forums       = Loading,
        _lm_organization = Loading,
        _lm_forum        = Loading
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
        , _l_lm_forums       = Loaded $ idmapFrom forumPackResponseForumId (forumPackResponses forums)
        }

    action_init_crud org_sid crud params = case crud of
      ShowS forum_sid   -> sync st org_sid forum_sid
      New               -> pure $ st{ _m_request = Just defaultForumRequest }
      EditS forum_sid   -> sync st org_sid forum_sid
      DeleteS forum_sid -> sync st org_sid forum_sid

    action_set_m_request request =
      pure $ st{
        _m_request = Just request
      }

    action_set_m_request_state m_req m_tag = pure $ st{ _m_request = m_req, _m_requestTag = m_tag }

    action_save = do
      let org_sid = "FIXME"
      case (_m_request, _lm_organization) of
        (Just forum_m_request, Loaded (Just OrganizationPackResponse{..})) -> do
          lr <- rd $ postForum_ByOrganizationId' organizationPackResponseOrganizationId forum_m_request
          rehtie lr (const $ pure st) $ \forum_response@ForumResponse{..} -> do
            forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForums org_sid (ShowS forumResponseName)) []
            pure st
        _            -> pure st

    action_edit edit_id = do
      let org_sid = "FIXME"
      case _m_request of
        Nothing            -> pure st
        Just forum_m_request -> do
          lr <- rd $ putForum' edit_id $ forum_m_request
          rehtie lr (const $ pure st) $ \forum_response@ForumResponse{..} -> do
            forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForums org_sid (ShowS forumResponseName)) []
            pure st



sync :: Store -> Text -> Text -> IO Store
sync st@Store{..} org_sid forum_sid = do
  lr <- runEitherT $ do
    organization@OrganizationPackResponse{..} <- mustPassT $ rd $ getOrganizationPack' org_sid
    forum        <- mustPassT $ rd $ ApiS.getForumPack_ByOrganizationId' forum_sid organizationPackResponseOrganizationId
    pure (organization, forum)
  rehtie lr (const $ pure st) $ \(organization@OrganizationPackResponse{..}, forum@ForumPackResponse{..}) -> do
    pure $ st{
      _m_request      = Just $ forumResponseToForumRequest forumPackResponseForum
    , _lm_organization = Loaded $ Just organization
    , _lm_forum        = Loaded $ Just forum
    }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _lm_organization  = Loaded Nothing,
  _l_lm_forums        = Loaded Map.empty,
  _lm_forum         = Loaded Nothing,
  _m_request       = Nothing,
  _m_requestTag    = Nothing
}



view_ :: CRUD -> HTMLEvent_
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "organizations" store $ \st@Store{..} crud ->
  mempty



viewIndex :: Store -> HTMLView_
viewIndex Store{..} = do
  Loading.loader2 _lm_organization _l_lm_forums $ \m_organization forums -> do
    case m_organization of
      Nothing           -> mempty
      Just organization -> viewIndex' organization forums



viewIndex' :: OrganizationPackResponse -> Map Int64 ForumPackResponse -> HTMLView_
viewIndex' org_pack@OrganizationPackResponse{..} forums_map = do
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
            ahrefClasses [B.listGroupItem] $ routeWith' $ OrganizationsForums organizationResponseName (ShowS forumResponseName)
    ) $ Map.elems forums_map

  where
  org@OrganizationResponse{..} = organizationPackResponseOrganization




viewShowS :: Loader (Maybe OrganizationPackResponse) -> Loader (Maybe ForumPackResponse) -> HTMLView_
viewShowS l_lm_organization l_lm_forum = do
  Loading.loader2 l_lm_organization l_lm_forum $ go
  where
  go Nothing Nothing = mempty
  go (Just organization@OrganizationPackResponse{..}) (Just forum@ForumPackResponse{..}) = do
    mempty




viewNew :: Maybe Text -> Maybe OrganizationRequest -> HTMLView_
viewNew m_tag m_m_request =
  mempty



viewEditS :: Maybe Text -> Maybe OrganizationRequest -> Loader (Maybe OrganizationPackResponse) -> HTMLView_
viewEditS m_tag m_m_request l_lm_organization_pack =
  mempty



viewMod :: TyCRUD -> Maybe Int64 -> Maybe Text -> OrganizationRequest -> HTMLView_
viewMod tycrud m_lm_organization_id m_tag request@OrganizationRequest{..} = do
  mempty



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
