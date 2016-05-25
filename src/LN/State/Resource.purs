module LN.State.Resource (
  ResourceRequestState,
  defaultResourceRequestState
) where



import LN.Internal.Resource (RType(..))



type ResourceRequestState = {
  rtype :: RType
}



defaultResourceRequestState :: ResourceRequestState
defaultResourceRequestState = {
  rtype: RTypeNONE
}
