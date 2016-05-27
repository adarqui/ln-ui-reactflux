module LN.State.Resource (
  ResourceRequestState,
  defaultResourceRequestState
) where



import LN.T (TyResourceType(..))



type ResourceRequestState = {
  source :: TyResourceType
}



defaultResourceRequestState :: ResourceRequestState
defaultResourceRequestState = {
  source: TySourceNone
}
