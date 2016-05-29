module LN.State.Leuron (
  LeuronRequestState,
  defaultLeuronRequestState,
  leuronRequestStateFromLeuronData
) where



import LN.Internal.Leuron ( defaultFact
                          , defaultFactList
                          , defaultCard
                          , defaultDCard
                          , defaultDCardX
                          , defaultAcronym
                          , defaultSynonym
                          , defaultAntonym
                          , defaultTemplate
                          , defaultImageAssociation
                          , defaultLinearDemo
                          , defaultTable
                          , defaultScript
                          , defaultQA)

import LN.T               ( TyLeuron(..)
                          , LeuronData(..)
                          , Fact
                          , FactList
                          , Card
                          , DCard
                          , DCardX
                          , Acronym
                          , Synonym
                          , Antonym
                          , Template
                          , ImageAssociation
                          , LinearDemo
                          , Table
                          , Script
                          , QA)



type LeuronRequestState = {
  ty                :: TyLeuron,
  fact              :: Fact,
  factList          :: FactList,
  factList_listItem :: String,
  card              :: Card,
  dcard             :: DCard,
  dcardx            :: DCardX,
  acronym           :: Acronym,
  synonym           :: Synonym,
  antonym           :: Antonym,
  template          :: Template,
  imageAssociation  :: ImageAssociation,
  linearDemo        :: LinearDemo,
  table             :: Table,
  qa                :: QA,
  ids               :: Array Int
}



defaultLeuronRequestState :: LeuronRequestState
defaultLeuronRequestState = {
  ty:                TyLnFact,
  fact:              defaultFact,
  factList:          defaultFactList,
  factList_listItem: "",
  card:              defaultCard,
  dcard:             defaultDCard,
  dcardx:            defaultDCardX,
  acronym:           defaultAcronym,
  synonym:           defaultSynonym,
  antonym:           defaultAntonym,
  template:          defaultTemplate,
  imageAssociation:  defaultImageAssociation,
  linearDemo:        defaultLinearDemo,
  table:             defaultTable,
  qa:                defaultQA,
  ids:               []
}



leuronRequestStateFromLeuronData :: LeuronData -> LeuronRequestState -> LeuronRequestState
leuronRequestStateFromLeuronData d st =
  case d of
       LnFact v             -> st{fact = v}
       LnFactList v         -> st{factList = v}
       LnCard v             -> st{card = v}
       LnDCard v            -> st{dcard = v}
       LnDCardX v           -> st{dcardx = v}
       LnAcronym v          -> st{acronym = v}
       LnSynonym v          -> st{synonym = v}
       LnAntonym v          -> st{antonym = v}
       LnTemplate v         -> st{template = v}
       LnImageAssociation v -> st{imageAssociation = v}
       LnLinearDemo v       -> st{linearDemo = v}
       LnTable v            -> st{table = v}
       LnQA v               -> st{qa = v}
       _                    -> st
