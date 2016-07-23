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
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Haskell.Helpers.Either          (mustPassT)
import           React.Flux                      hiding (view)
import qualified React.Flux                      as RF

import           LN.Api                          (getOrganizationPacks,
                                                  getOrganizationsCount')
import           LN.T.Organization               (OrganizationResponse (..))
import           LN.T.Pack.Organization          (OrganizationPackResponse (..), OrganizationPackResponses (..))
import           LN.UI.App.PageNumbers           (runPageInfo)
import qualified LN.UI.App.PageNumbers           as PageNumbers
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref)
import           LN.UI.Router.Class.CRUD         (CRUD (..))
import           LN.UI.Router.Class.Param        (Params)
import           LN.UI.Router.Class.Route        (Route (..), RouteWith (..),
                                                  routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)
import LN.UI.App.Types (UsersMap)



data Store = Store {
  _pageInfo      :: PageInfo,
  _organizations :: Map Int64 OrganizationPackResponse
}



data Action
  = Init Params
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Organizations"

    case action of
      Nop              -> pure st
      Init params  -> actions_init params

    where
    actions_init params = do
      let
        page_info   = pageInfoFromParams params
        params_list = paramsFromPageInfo page_info
      lr <- runEitherT $ do
        count         <- mustPassT $ rd $ getOrganizationsCount'
        organizations <- mustPassT $ rd $ getOrganizationPacks params_list
        pure (count, organizations)
      rehtie lr (const $ pure st) $ \(count, organization_packs) -> do
        let new_page_info = runPageInfo count page_info
        pure $ st{ _organizations = idmapFrom organizationPackResponseOrganizationId (organizationPackResponses organization_packs)
                 , _pageInfo = new_page_info }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _pageInfo      = defaultPageInfo,
  _organizations = Map.empty
}



view_ :: UsersMap -> ReactElementM eventHandler ()
view_ users_map =
  RF.view view users_map mempty



view :: ReactView UsersMap
view = defineControllerView "organizations" store $ \Store{..} users_map ->
  div_ $ do
    h1_ "Organizations"
    PageNumbers.view_ (_pageInfo, routeWith' $ Organizations Index)
    ul_ $ do
      mapM_ (\OrganizationPackResponse{..} -> do
        li_ $ ahref $ routeWith' $ Organizations (ShowS $ organizationResponseName organizationPackResponseOrganization)
        ) _organizations
