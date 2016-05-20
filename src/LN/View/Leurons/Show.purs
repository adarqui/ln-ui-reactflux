module LN.View.Leurons.Show (
  renderView_Leurons_Show
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, ($))

import LN.Input.Types                  (Input)
import LN.Router.Internal              (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.T



renderView_Leurons_Show :: String -> State -> ComponentHTML Input
renderView_Leurons_Show leuron_sid st =

  case st.currentLeuron of
       Nothing   -> H.div_ [H.text "Leuron Unavailable"]
       Just pack -> renderView_Leurons_Show' pack st



renderView_Leurons_Show' :: LeuronPackResponse -> State -> ComponentHTML Input
renderView_Leurons_Show' pack st =

  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [H.text $ show leuron.id]
--      H.p [P.class_ B.textCenter] [H.text (leuron.description)]
    ],
    H.div [P.class_ B.container] [
      renderLeuron leuron'
    ]
  ]

 where
 leuron  = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
 leuron' = pack ^. _LeuronPackResponse .. leuron_



renderLeuron :: LeuronResponse -> ComponentHTML Input
renderLeuron leuron =
  H.div_ [
    renderLeuronData leuron',
    renderLeuronSection leuron',
    renderLeuronExamples leuron',
    renderLeuronCategories leuron'
  ]
  where
  leuron' = leuron ^. _LeuronResponse



renderLeuronExamples :: LeuronResponseR -> ComponentHTML Input
renderLeuronExamples leuron =
  case leuron.examples of
    Nothing       -> H.div_ []
    Just examples ->
      H.div_ [
        H.p_ [
          H.h2_ [H.text "Examples"],
          H.ul_ $ map (\example -> H.li_ [H.pre_ [H.text example]]) examples
        ]
      ]



renderLeuronSection :: LeuronResponseR -> ComponentHTML Input
renderLeuronSection leuron =
  case leuron.section of
    Nothing      -> H.div_ []
    Just section ->
      H.div_ [
        H.p_ [
          H.h2_ [H.text "Section"],
          H.pre_ [H.text section]
        ]
      ]



renderLeuronCategories :: LeuronResponseR -> ComponentHTML Input
renderLeuronCategories leuron =
  case leuron.categories of
    [] -> H.div_ []
    _  ->
      H.div_ [
        H.p_ [
          H.h2_ [H.text "Categories"],
          H.ul_ $ map (\category -> H.li_ [H.pre_ [H.text $ show category]]) leuron.categories
        ]
      ]



renderLeuronData :: LeuronResponseR -> ComponentHTML Input
renderLeuronData leuron =
  case leuron.dataP of
    LnFact fact          -> renderLeuronData_Fact leuron (unwrapFact fact)
    LnFactList fact_list -> renderLeuronData_FactList leuron (unwrapFactList fact_list)
    LnCard card          -> renderLeuronData_Card leuron (unwrapCard card)
    LnDCard dcard        -> renderLeuronData_DCard leuron (unwrapDCard dcard)
    LnDCardX dcardx      -> renderLeuronData_DCardX leuron (unwrapDCardX dcardx)
    LnAcronym acronym    -> renderLeuronData_Acronym leuron (unwrapAcronym acronym)
    _                    -> renderLeuronData_Unknown leuron



renderLeuronData_Fact :: LeuronResponseR -> FactR -> ComponentHTML Input
renderLeuronData_Fact leuron fact =
  H.div_ [
    H.p_ [
      H.h2_ [H.text "Fact"],
      H.pre_ [H.text fact.text]
    ]
  ]




renderLeuronData_FactList :: LeuronResponseR -> FactListR -> ComponentHTML Input
renderLeuronData_FactList leuron fact_list =
  H.div_ [
    H.p_ [
      H.h2_ [H.text "Fact"],
      H.pre_ [H.text fact_list.fact]
    ],
    H.p_ [
      H.ul_ $ map (\fact -> H.li_ [H.pre_ [H.text fact]]) fact_list.list
    ]
  ]



renderLeuronData_Card :: LeuronResponseR -> CardR -> ComponentHTML Input
renderLeuronData_Card leuron card =
  H.div_ [
    H.p_ [H.h2_ [H.text "Front"], H.pre_ [H.text card.front]],
    H.p_ [H.h2_ [H.text "Back"], H.pre_ [H.text card.back]]
  ]



renderLeuronData_DCard :: LeuronResponseR -> DCardR -> ComponentHTML Input
renderLeuronData_DCard leuron dcard =
  H.div_ [
    H.p_ [H.h2_ [H.text "Front"], H.pre_ [H.text dcard.front]],
    H.p_ [H.h2_ [H.text "Back"], H.pre_ [H.text dcard.back]]
  ]



renderLeuronData_DCardX :: LeuronResponseR -> DCardXR -> ComponentHTML Input
renderLeuronData_DCardX leuron dcardx =
  H.div_ [
    H.p_ [H.h2_ [H.text "Front"],
      H.ul_ $ map (\front -> H.li_ [H.pre_ [H.text front]]) dcardx.front
    ],
    H.p_ [H.h2_ [H.text "Back"],
      H.ul_ $ map (\back -> H.li_ [H.pre_ [H.text back]]) dcardx.back
    ]
  ]



renderLeuronData_Acronym :: LeuronResponseR -> AcronymR -> ComponentHTML Input
renderLeuronData_Acronym leuron acronym =
  H.div_ [
    H.p_ [H.h2_ [H.text "Abbrv"], H.pre_ [H.text acronym.abbreviation]],
    H.p_ [H.h2_ [H.text "Meaning"], H.pre_ [H.text acronym.meaning]]
  ]



renderLeuronData_Unknown :: LeuronResponseR -> ComponentHTML Input
renderLeuronData_Unknown leuron =
  H.div_ [H.text "unknown"]
