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
  view,
  view_
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
import           LN.UI.Router.Class.CRUD         (CRUD (..))
import           LN.UI.Router.Class.Route        (Route (..), routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  paramsFromPageInfo)



data Store = Store {
  _pageInfo      :: PageInfo,
  _organizations :: Map Int64 OrganizationPackResponse
}



data Action
  = Init PageInfo
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Organizations"

    case action of
      Nop            -> pure st
      Init page_info -> actions_init page_info

    where
    actions_init page_info = do
      lr <- runEitherT $ do
        count         <- mustPassT $ rd $ getOrganizationsCount'
        organizations <- mustPassT $ rd $ getOrganizationPacks $ paramsFromPageInfo page_info
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



view :: ReactView ()
view = defineControllerView "organizations" store $ \Store{..} _ ->
  div_ $ do
    h1_ "Organizations"
    PageNumbers.view_ (_pageInfo, routeWith' $ Organizations Index)
    ul_ $ do
      mapM_ (\OrganizationPackResponse{..} -> li_ $ p_ $ elemShow (organizationResponseName organizationPackResponseOrganization)) _organizations



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty
