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
import qualified LN.Api.String                   as ApiS (getForumPack_ByOrganizationId', getOrganizationPack',
                                                          getOrganization')
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
import           LN.UI.Helpers.ReactFluxDOM      (ahref, ahrefName, className_,
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
  _forums       :: Loader (Map Int64 ForumPackResponse),
  _organization :: Loader (Maybe OrganizationPackResponse),
  _forum        :: Loader (Maybe ForumPackResponse),
  _request      :: Maybe ForumRequest,
  _requestTag   :: Maybe Text
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
      SetRequest request          -> action_set_request request
      SetRequestState m_req m_tag -> action_set_request_state m_req m_tag
      Save                        -> action_save
      Edit edit_id                -> action_edit edit_id

    where
    action_load = do
      pure $ st{
        _forums       = Loading,
        _organization = Loading,
        _forum        = Loading
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
          _organization = Loaded $ Just organization
        , _forums       = Loaded $ idmapFrom forumPackResponseForumId (forumPackResponses forums)
        }

    action_init_crud org_sid crud params = case crud of
      ShowS forum_sid   -> sync st org_sid forum_sid
      New               -> pure $ st{ _request = Just defaultForumRequest }
      EditS forum_sid   -> sync st org_sid forum_sid
      DeleteS forum_sid -> sync st org_sid forum_sid

    action_set_request request =
      pure $ st{
        _request = Just request
      }

    action_set_request_state m_req m_tag = pure $ st{ _request = m_req, _requestTag = m_tag }

    action_save = do
      let org_sid = "FIXME"
      case (_request, _organization) of
        (Just forum_request, Loaded (Just OrganizationPackResponse{..})) -> do
          lr <- rd $ postForum_ByOrganizationId' organizationPackResponseOrganizationId forum_request
          rehtie lr (const $ pure st) $ \forum_response@ForumResponse{..} -> do
            forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (OrganizationsForums org_sid (ShowS forumResponseName)) []
            pure st
        _            -> pure st

    action_edit edit_id = do
      let org_sid = "FIXME"
      case _request of
        Nothing            -> pure st
        Just forum_request -> do
          lr <- rd $ putForum' edit_id $ forum_request
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
      _request      = Just $ forumResponseToForumRequest forumPackResponseForum
    , _organization = Loaded $ Just organization
    , _forum        = Loaded $ Just forum
    }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _organization  = Loaded Nothing,
  _forums        = Loaded Map.empty,
  _forum         = Loaded Nothing,
  _request       = Nothing,
  _requestTag    = Nothing
}



view_ :: CRUD -> HTMLEvent_
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "organizations" store $ \st@Store{..} crud ->
  mempty



viewIndex :: Store -> HTMLView_
viewIndex Store{..} = do
  Loading.loader2 _organization _forums $ \organization forums -> do
    mempty



viewShowS :: Loader (Maybe OrganizationPackResponse) -> Loader (Maybe ForumPackResponse) -> HTMLView_
viewShowS l_organization l_forum = do
  Loading.loader2 l_organization l_forum $ go
  where
  go Nothing Nothing = mempty
  go (Just organization@OrganizationPackResponse{..}) (Just forum@ForumPackResponse{..}) = do
    mempty




viewNew :: Maybe Text -> Maybe OrganizationRequest -> HTMLView_
viewNew m_tag m_request =
  mempty



viewEditS :: Maybe Text -> Maybe OrganizationRequest -> Loader (Maybe OrganizationPackResponse) -> HTMLView_
viewEditS m_tag m_request l_organization_pack =
  mempty



viewMod :: TyCRUD -> Maybe Int64 -> Maybe Text -> OrganizationRequest -> HTMLView_
viewMod tycrud m_organization_id m_tag request@OrganizationRequest{..} = do
  mempty



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
