module LN.State.Leuron (
  LeuronRequestState,
  defaultLeuronRequestState,
  leuronRequestStateFromLeuronData
) where



import LN.Internal.Leuron ( defaultFact
                          , defaultFactList
                          , defaultCard
                          , defaultDCard)

import LN.T               ( TyLeuron(..)
                          , LeuronData(..)
                          , Fact
                          , FactList
                          , Card
                          , DCard)



type LeuronRequestState = {
  ty                :: TyLeuron,
  fact              :: Fact,
  factList          :: FactList,
  factList_listItem :: String,
  card              :: Card,
  dcard             :: DCard,
  ids               :: Array Int
}



defaultLeuronRequestState :: LeuronRequestState
defaultLeuronRequestState = {
  ty:  TyLnFact,
  fact: defaultFact,
  factList: defaultFactList,
  factList_listItem: "",
  card: defaultCard,
  dcard: defaultDCard,
  ids: []
}



leuronRequestStateFromLeuronData :: LeuronData -> LeuronRequestState -> LeuronRequestState
leuronRequestStateFromLeuronData d st =
  case d of
       LnFact v         -> st{fact = v}
       LnFactList v     -> st{factList = v}
       LnCard v         -> st{card = v}
       LnDCard v        -> st{dcard = v}
       _                -> st
