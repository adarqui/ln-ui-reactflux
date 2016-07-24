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
                                                  getOrganizationsCount')
import           LN.Api.String                   (getOrganizationPack')
import           LN.Generate.Default             (defaultOrganizationRequest)
import           LN.T.Organization               (OrganizationRequest (..),
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
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref)
import           LN.UI.Router                    (CRUD (..), Params, Route (..),
                                                  RouteWith (..), TyCRUD (..),
                                                  linkName, routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)
import           LN.UI.View.Field



data Store = Store {
  _pageInfo      :: PageInfo,
  _organizations :: Loader (Map Int64 OrganizationPackResponse),
  _organization  :: Loader (Maybe OrganizationPackResponse),
  _request       :: Maybe OrganizationRequest,
  _requestTag    :: Maybe Text
}



data Action
  = Load
  | Init       CRUD Params
  | SetRequest OrganizationRequest
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Organizations"

    case action of
      Nop                -> pure st
      Load               -> action_load
      Init crud params   -> action_init crud params
      SetRequest request -> action_set_request request

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

    action_set_request request =
      pure $ st{
        _request = Just request
      }



sync :: Store -> Text -> IO Store
sync st@Store{..} org_sid = do
  lr <- runEitherT $ do
    organization <- mustPassT $ rd $ getOrganizationPack' org_sid
    pure organization
  rehtie lr (const $ pure st) $ \organization@OrganizationPackResponse{..} -> do
    pure $ st{
--      request = Just organizationResponseToOrganizationRequest organizationPackResponseOrganization
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
  _requestTag    = Nothing
}



view_ :: CRUD -> ReactElementM eventHandler ()
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "organizations" store $ \st@Store{..} crud ->
  case crud of
    Index           -> viewIndex st
    ShowS org_sid   -> viewShowS _organization
    New             -> viewNew _request
    EditS org_sid   -> viewEditS _request _organization
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



viewNew :: Maybe OrganizationRequest -> ReactElementM ViewEventHandler ()
viewNew m_request =
  ebyam m_request mempty $ \request -> viewMod TyCreate Nothing request



viewEditS :: Maybe OrganizationRequest -> Loader (Maybe OrganizationPackResponse) -> ReactElementM ViewEventHandler ()
viewEditS m_request l_organization_pack =
  Loading.loader1 l_organization_pack $ \m_organization_pack -> do
    case (m_request, m_organization_pack) of
      (Just request, Just OrganizationPackResponse{..}) -> viewMod TyUpdate (Just organizationPackResponseOrganizationId) request
      (_, _) -> mempty



viewMod :: TyCRUD -> Maybe Int64 -> OrganizationRequest -> ReactElementM ViewEventHandler ()
viewMod tycrud m_organization_id request@OrganizationRequest{..} = do
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

  -- -- , icon

  -- , tagsField
  --     organization.tags
  --     (maybe "" id org_req_st.currentTag)
  --     (cOrganizationMod <<< SetTag)
  --     (cOrganizationMod AddTag)
  --     (cOrganizationMod <<< DeleteTag)
  --     (cOrganizationMod ClearTags)

  -- , buttons_CreateEditCancel m_organization_id (cOrganizationMod Create) (cOrganizationMod <<< EditP) About

  -- ]
  -- where
  -- organization        = unwrapOrganizationRequest organization_req



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
