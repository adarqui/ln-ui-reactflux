{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Organizations (
  OrganizationsStore,
  defaultOrganizationsStore,
  OrganizationsAction (..),
  organizationsStore,
  organizationsView,
  organizationsView_
) where



import           Control.DeepSeq                 (NFData)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           React.Flux                      hiding (view)
import qualified React.Flux                      as RF

import           LN.Api                          (getOrganizationPacks)
import           LN.T.Pack.Organization          (OrganizationPackResponse (..))
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  paramsFromPageInfo)



data OrganizationsStore = OrganizationsStore {
  organizationsStore_PageInfo      :: PageInfo,
  organizationsStore_Organizations :: Map Int OrganizationPackResponse
}



data OrganizationsAction
  = Organizations_Init PageInfo
  | Organizations_Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData OrganizationsStore where
  type StoreAction OrganizationsStore = OrganizationsAction
  transform action st = do
    putStrLn "Organizations"

    case action of
      Organizations_Init page_info -> do
        lr <- rd $ getOrganizationPacks $ paramsFromPageInfo page_info
        pure $ st{ organizationsStore_PageInfo = page_info }



organizationsStore :: ReactStore OrganizationsStore
organizationsStore = mkStore defaultOrganizationsStore



defaultOrganizationsStore :: OrganizationsStore
defaultOrganizationsStore = OrganizationsStore {
  organizationsStore_PageInfo      = defaultPageInfo,
  organizationsStore_Organizations = Map.empty
}



organizationsView :: ReactView ()
organizationsView = defineControllerView "organizations" organizationsStore $ \st _ ->
  div_ $ p_ $ elemText "Organizations Index"



organizationsView_ :: ReactElementM eventHandler ()
organizationsView_ =
  RF.view organizationsView () mempty
