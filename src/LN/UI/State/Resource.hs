module LN.UI.State.Resource (
  ResourceRequestState (..),
  defaultResourceRequestState
) where



import           LN.T (TyResourceType (..))



data ResourceRequestState = ResourceRequestState {
  source :: TyResourceType
}



defaultResourceRequestState :: ResourceRequestState
defaultResourceRequestState = ResourceRequestState {
  source = TySourceNone
}
