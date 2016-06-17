module LN.State.Organization (
  OrganizationRequestState,
  defaultOrganizationRequestState
) where



import Data.Maybe (Maybe(..))



type OrganizationRequestState = {
  currentTag :: Maybe String
}



defaultOrganizationRequestState :: OrganizationRequestState
defaultOrganizationRequestState = {
  currentTag: Nothing
}
