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
import           Data.Int                        (Int64)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
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
import           LN.T.Organization               (OrganizationRequest (..),
                                                  OrganizationResponse (..))
import           LN.T.Pack.Organization          (OrganizationPackResponse (..), OrganizationPackResponses (..))
import           LN.T.Size                       (Size (..))
import           LN.T.User                       (UserSanitizedResponse (..))
import LN.UI.App.Loading (Loader(..))
import qualified LN.UI.App.Loading as Loading
import qualified LN.UI.App.Delete                as Delete
import qualified LN.UI.App.Gravatar              as Gravatar
import           LN.UI.App.PageNumbers           (runPageInfo)
import qualified LN.UI.App.PageNumbers           as PageNumbers
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref)
import           LN.UI.Router                    (CRUD (..), TyCRUD(..), Params, Route (..),
                                                  RouteWith (..), routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)



data Store = Store {
  _pageInfo      :: PageInfo,
  _organizations :: Loader (Map Int64 OrganizationPackResponse),
  _organization  :: Loader (Maybe OrganizationPackResponse),
  _request       :: Maybe OrganizationRequest,
  _requestTag    :: Maybe Text
}



data Action
  = Load
  | Init CRUD Params
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Organizations"

    case action of
      Nop              -> pure st
      Load             -> action_load
      Init crud params -> actions_init crud params

    where
    action_load = do
      pure $ st{
        _organizations = Loading,
        _organization  = Loading
      }

    actions_init crud params = case crud of
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
      New             -> pure st
      EditS org_sid   -> sync st org_sid
      DeleteS org_sid -> sync st org_sid



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
    ShowS org_sid   -> viewShowS org_sid
    New             -> viewNew
    EditS org_sid   -> viewEditS org_sid
    DeleteS org_sid -> Delete.view_



viewIndex :: Store -> ReactElementM ViewEventHandler ()
viewIndex Store{..} = do
  Loading.loader1 _organizations $ \organizations -> do
    div_ $ do
      h1_ "Organizations"
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



viewShowS :: Text -> ReactElementM ViewEventHandler ()
viewShowS org_sid = p_ $ elemText "show"



viewNew :: ReactElementM ViewEventHandler ()
viewNew = p_ $ elemText "new"



viewEditS :: Text -> ReactElementM ViewEventHandler ()
viewEditS org_sid = p_ $ elemText "edit"



viewMod :: TyCRUD -> Maybe Int64 -> Store -> ReactElementM ViewEventHandler ()
viewMod tycrud m_organization_id st@Store{..} = pure ()
  -- case st.currentOrganizationRequest, st.currentOrganizationRequestSt, getLoading l_currentOrganization st.loading of
  --   _, _, true                                    -> renderLoading
  --   Just organization_req, Just org_req_st, false -> renderView_Organizations_Mod' crud m_organization_id organization_req org_req_st
  --   _, _, false                                   -> H.div_ [H.p_ [H.text "Organizations_Mod: unexpected error."]]
