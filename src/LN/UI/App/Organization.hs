{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Organization (
  Store,
  defaultStore,
  Action (..),
  store,
  view,
  view_
) where



import           Control.DeepSeq         (NFData)
import           Data.Text               (Text)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           React.Flux              hiding (view)
import qualified React.Flux              as RF

import qualified LN.UI.App.Delete        as Delete
import LN.T.Organization (OrganizationRequest (..), OrganizationResponse(..))
import LN.T.Pack.Organization (OrganizationPackResponse(..))
import           LN.UI.Router.Class.CRUD
import           LN.UI.Router.Class.Param
import LN.UI.Router.Class.Route (RouteWith)



data Store = Store {
  request    :: Maybe OrganizationRequest,
  organization :: Maybe OrganizationPackResponse,
  currentTag :: Maybe Text
} deriving (Show, Typeable, Generic, NFData)



data Action
  = Init CRUD Params
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Organization"
    case action of
      Init crud params -> action_init crud params
      Nop -> pure st
    where
    action_init crud params = case crud of
      ShowS org_sid   -> pure st
      New             -> pure st
      EditS org_sid   -> pure st
      DeleteS org_sid -> pure st



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  request      = Nothing,
  organization = Nothing,
  currentTag   = Nothing
}



view_ :: CRUD -> ReactElementM eventHandler ()
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "organization" store $ \st crud ->
  case crud of
    ShowS org_sid   -> viewShowS org_sid
    New             -> viewNew
    EditS org_sid   -> viewEditS org_sid
    DeleteS org_sid -> Delete.view_



viewShowS :: Text -> ReactElementM ViewEventHandler ()
viewShowS org_sid = p_ $ elemText "show"



viewNew :: ReactElementM ViewEventHandler ()
viewNew = p_ $ elemText "new"



viewEditS :: Text -> ReactElementM ViewEventHandler ()
viewEditS org_sid = p_ $ elemText "edit"
