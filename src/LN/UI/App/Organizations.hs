{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Organizations (
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

import           LN.Api                          (getOrganizationPacks,
                                                  getOrganizationsCount',
                                                  postOrganization',
                                                  putOrganization')
import           LN.Api.String                   (getOrganizationPack')
import           LN.Generate.Default             (defaultOrganizationRequest)
import           LN.T.Convert                    (organizationResponseToOrganizationRequest)
import           LN.T.Organization               (OrganizationRequest (..),
                                                  OrganizationResponse (..),
                                                  OrganizationResponse (..))
import           LN.T.Pack.Organization          (OrganizationPackResponse (..), OrganizationPackResponses (..))
import           LN.T.Size                       (Size (..))
import           LN.T.User                       (UserSanitizedResponse (..))
import qualified LN.UI.App.Delete                as Delete
import qualified LN.UI.App.Gravatar              as Gravatar
import           LN.UI.App.Loading               (Loader (..))
import qualified LN.UI.App.Loading               as Loading
import           LN.UI.App.PageNumbers           (runPageInfo)
import qualified LN.UI.App.PageNumbers           as PageNumbers
import qualified LN.UI.App.Route                 as Route
import           LN.UI.Helpers.DataList          (deleteNth)
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref)
import           LN.UI.Router                    (CRUD (..), Params, Route (..),
                                                  RouteWith (..), TyCRUD (..),
                                                  linkName, routeWith,
                                                  routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)
import           LN.UI.View.Button               (createButtonsCreateEditCancel)
import           LN.UI.View.Field



data Store = Store {
  _pageInfo      :: PageInfo,
  _organizations :: Loader (Map Int64 OrganizationPackResponse),
  _organization  :: Loader (Maybe OrganizationPackResponse),
  _request       :: Maybe OrganizationRequest,
  _requestTag    :: Maybe Text,
  _requestEmail  :: Text
}



data Action
  = Load
  | Init            CRUD Params
  | SetEmail        Text
  | SetRequest      OrganizationRequest
  | SetRequestState (Maybe OrganizationRequest) (Maybe Text)
  | Save
  | Edit            Int64
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do
    putStrLn "Organizations"

    case action of
      Nop                         -> pure st
      Load                        -> action_load
      Init crud params            -> action_init crud params
      SetEmail email              -> action_set_email email
      SetRequest request          -> action_set_request request
      SetRequestState m_req m_tag -> action_set_request_state m_req m_tag
      Save                        -> action_save
      Edit edit_id                -> action_edit edit_id

    where
    action_load = do
      pure $ st{
        _organizations = Loading,
        _organization  = Loading
      }

    action_init crud params = case crud of
      Index -> action_init_index params
      _     -> action_init_crud crud params

    action_init_index params = do
      let
        page_info   = pageInfoFromParams params
        params_list = paramsFromPageInfo page_info
      lr <- runEitherT $ do
        count         <- mustPassT $ rd $ getOrganizationsCount'
        organizations <- mustPassT $ rd $ getOrganizationPacks params_list
        pure (count, organizations)
      rehtie lr (const $ pure st) $ \(count, organization_packs) -> do
        let new_page_info = runPageInfo count page_info
        pure $ st{ _organizations = Loaded $ idmapFrom organizationPackResponseOrganizationId (organizationPackResponses organization_packs)
                 , _pageInfo = new_page_info }

    action_init_crud crud params = case crud of
      ShowS org_sid   -> sync st org_sid
      New             -> pure $ st{ _request = Just defaultOrganizationRequest }
      EditS org_sid   -> sync st org_sid
      DeleteS org_sid -> sync st org_sid

    action_set_email email =
      pure $ st{
        _requestEmail = email
      }

    action_set_request request =
      pure $ st{
        _request = Just request
      }

    action_set_request_state m_req m_tag = pure $ st{ _request = m_req, _requestTag = m_tag }

    action_save = do
      case _request of
        Nothing                   -> pure st
        Just organization_request -> do
          lr <- rd $ postOrganization' $ organization_request { organizationRequestEmail = _requestEmail }
          rehtie lr (const $ pure st) $ \organization_response@OrganizationResponse{..} -> do
            forkIO $ executeAction $ SomeStoreAction Route.store $ Route.Goto $ routeWith (Organizations (ShowS $ organizationResponseName)) []
            pure st

    action_edit edit_id = do
      pure st



sync :: Store -> Text -> IO Store
sync st@Store{..} org_sid = do
  lr <- runEitherT $ do
    organization <- mustPassT $ rd $ getOrganizationPack' org_sid
    pure organization
  rehtie lr (const $ pure st) $ \organization@OrganizationPackResponse{..} -> do
    pure $ st{
      _request      = Just $ organizationResponseToOrganizationRequest organizationPackResponseOrganization,
      _organization = Loaded $ Just organization
    }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _pageInfo      = defaultPageInfo,
  _organizations = Loaded Map.empty,
  _organization  = Loaded Nothing,
  _request       = Nothing,
  _requestTag    = Nothing,
  _requestEmail  = ""
}



view_ :: CRUD -> ReactElementM eventHandler ()
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "organizations" store $ \st@Store{..} crud ->
  case crud of
    Index           -> viewIndex st
    ShowS org_sid   -> viewShowS _organization
    New             -> viewNew _requestTag _request
    EditS org_sid   -> viewEditS _requestTag _request _organization
    DeleteS org_sid -> Delete.view_



viewIndex :: Store -> ReactElementM ViewEventHandler ()
viewIndex Store{..} = do
  Loading.loader1 _organizations $ \organizations -> do
    div_ $ do
      h1_ "Organizations"
      ahref $ routeWith' $ Organizations New
      PageNumbers.view_ (_pageInfo, routeWith' $ Organizations Index)
      ul_ $ do
        mapM_ (\OrganizationPackResponse{..} -> do
          li_ $ do
            ul_ $ do
              li_ $ p_ $ elemText $ organizationResponseDisplayName organizationPackResponseOrganization
              li_ $ ahref $ routeWith' $ Organizations (ShowS $ organizationResponseName organizationPackResponseOrganization)
              li_ $ p_ $ elemText $ userSanitizedResponseName organizationPackResponseUser
              li_ $ Gravatar.viewUser_ XSmall organizationPackResponseUser
          ) organizations



viewShowS :: Loader (Maybe OrganizationPackResponse) -> ReactElementM ViewEventHandler ()
viewShowS l_organization_pack = do
  p_ $ elemText "show"



viewNew :: Maybe Text -> Maybe OrganizationRequest -> ReactElementM ViewEventHandler ()
viewNew m_tag m_request =
  ebyam m_request mempty $ \request -> viewMod TyCreate Nothing m_tag request



viewEditS :: Maybe Text -> Maybe OrganizationRequest -> Loader (Maybe OrganizationPackResponse) -> ReactElementM ViewEventHandler ()
viewEditS m_tag m_request l_organization_pack =
  Loading.loader1 l_organization_pack $ \m_organization_pack -> do
    case (m_request, m_organization_pack) of
      (Just request, Just OrganizationPackResponse{..}) -> viewMod TyUpdate (Just organizationPackResponseOrganizationId) m_tag request
      (_, _) -> mempty



viewMod :: TyCRUD -> Maybe Int64 -> Maybe Text -> OrganizationRequest -> ReactElementM ViewEventHandler ()
viewMod tycrud m_organization_id m_tag request@OrganizationRequest{..} = do
  div_ $ do
    h1_ $ elemText $ linkName tycrud <> " Organization"

    mandatoryNameField organizationRequestDisplayName
      (\input -> dispatch $ SetRequest $ request{organizationRequestDisplayName = input})

    optionalDescriptionField organizationRequestDescription
      (\input -> dispatch $ SetRequest $ request{organizationRequestDescription = Just input})
      (dispatch $ SetRequest $ request{organizationRequestDescription = Nothing})

    mandatoryCompanyField organizationRequestCompany
      (\input -> dispatch $ SetRequest $ request{organizationRequestCompany = input})

    mandatoryLocationField organizationRequestLocation
      (\input -> dispatch $ SetRequest $ request{organizationRequestLocation = input})

    mandatoryMembershipField organizationRequestMembership
      (\input -> dispatch $ SetRequest $ request{organizationRequestMembership = input})

    mandatoryVisibilityField organizationRequestVisibility
      (\input -> dispatch $ SetRequest $ request{organizationRequestVisibility = input})

    -- icon

    tagsField
       organizationRequestTags
       (maybe ""  id m_tag)
       (\input -> dispatch $ SetRequestState (Just request) (Just input))
       (dispatch $ SetRequestState (Just $ request{organizationRequestTags = maybe organizationRequestTags (\tag -> organizationRequestTags <> [tag]) m_tag}) Nothing)
       (\idx -> dispatch $ SetRequest $ request{organizationRequestTags = deleteNth idx organizationRequestTags})
       (dispatch $ SetRequest $ request{organizationRequestTags = []})

    createButtonsCreateEditCancel
      m_organization_id
      (dispatch $ Save)
      (\edit_id -> dispatch $ Edit edit_id)
      (routeWith' Home)



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
