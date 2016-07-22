{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.App.Organization (
  OrganizationStore,
  defaultOrganizationStore,
  OrganizationAction (..),
  organizationStore,
  organizationView,
  organizationView_
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF
import LN.UI.Router.Class.CRUD



data OrganizationStore = OrganizationStore
  deriving (Show, Typeable, Generic, NFData)



data OrganizationAction = OrganizationAction
  deriving (Show, Typeable, Generic, NFData)



instance StoreData OrganizationStore where
  type StoreAction OrganizationStore = OrganizationAction
  transform action st = do
    putStrLn "Organization"
    pure OrganizationStore



organizationStore :: ReactStore OrganizationStore
organizationStore = mkStore defaultOrganizationStore



defaultOrganizationStore :: OrganizationStore
defaultOrganizationStore = OrganizationStore



organizationView :: ReactView CRUD
organizationView = defineControllerView "organization" organizationStore $ \st _ ->
  div_ $ p_ $ elemText "Welcome to LN!"



organizationView_ :: CRUD -> ReactElementM eventHandler ()
organizationView_ crud =
  RF.view organizationView crud mempty
