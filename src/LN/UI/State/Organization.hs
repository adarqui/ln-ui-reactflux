module LN.UI.State.Organization (
  OrganizationRequestState (..),
  defaultOrganizationRequestState
) where



import           Data.Maybe (Maybe (..))



data OrganizationRequestState = OrganizationRequestState {
  currentTag :: Maybe String
}



defaultOrganizationRequestState :: OrganizationRequestState
defaultOrganizationRequestState = OrganizationRequestState {
  currentTag = Nothing
}
