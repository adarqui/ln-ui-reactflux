module LN.State.Leuron (
  LeuronRequestState,
  defaultLeuronRequestState
) where



import LN.T (TyLeuron(..))



type LeuronRequestState = {
  ty        :: TyLeuron,
  ids       :: Array Int
}



defaultLeuronRequestState :: LeuronRequestState
defaultLeuronRequestState = {
  ty:  TyLnFact,
  ids: []
}
