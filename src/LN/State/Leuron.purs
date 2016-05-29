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
  exampleItem       :: String,
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
  exampleItem:       "",
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
       LnFact v             -> st{fact = v,             ty=TyLnFact}
       LnFactList v         -> st{factList = v,         ty=TyLnFactList}
       LnCard v             -> st{card = v,             ty=TyLnCard}
       LnDCard v            -> st{dcard = v,            ty=TyLnDCard}
       LnDCardX v           -> st{dcardx = v,           ty=TyLnDCardX}
       LnAcronym v          -> st{acronym = v,          ty=TyLnAcronym}
       LnSynonym v          -> st{synonym = v,          ty=TyLnSynonym}
       LnAntonym v          -> st{antonym = v,          ty=TyLnAntonym}
       LnTemplate v         -> st{template = v,         ty=TyLnTemplate}
       LnImageAssociation v -> st{imageAssociation = v, ty=TyLnImageAssociation}
       LnLinearDemo v       -> st{linearDemo = v,       ty=TyLnLinearDemo}
       LnTable v            -> st{table = v,            ty=TyLnTable}
       LnQA v               -> st{qa = v,               ty=TyLnQA}
       _                    -> st
