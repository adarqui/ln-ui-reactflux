{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.Organizations (
  OrganizationsStore,
  defaultOrganizationsStore,
  OrganizationsAction (..),
  organizationsStore,
  organizationsView,
  organizationsView_
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF



data OrganizationsStore = OrganizationsStore
  deriving (Show, Typeable, Generic, NFData)



data OrganizationsAction = OrganizationsAction
  deriving (Show, Typeable, Generic, NFData)



instance StoreData OrganizationsStore where
  type StoreAction OrganizationsStore = OrganizationsAction
  transform action st = do
    putStrLn "Organizations"
    pure OrganizationsStore



organizationsStore :: ReactStore OrganizationsStore
organizationsStore = mkStore defaultOrganizationsStore



defaultOrganizationsStore :: OrganizationsStore
defaultOrganizationsStore = OrganizationsStore



organizationsView :: ReactView ()
organizationsView = defineControllerView "organizations" organizationsStore $ \st _ ->
  div_ $ p_ $ elemText "Welcome to LN!"



organizationsView_ :: ReactElementM eventHandler ()
organizationsView_ =
  RF.view organizationsView () mempty
