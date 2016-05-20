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
renderLeuron ln =
  case leuron.dataP of
    LnFact fact          -> renderLeuron_Fact leuron (unwrapFact fact)
    LnFactList fact_list -> renderLeuron_FactList leuron (unwrapFactList fact_list)
    LnCard card          -> renderLeuron_Card leuron (unwrapCard card)
    LnDCard dcard        -> renderLeuron_DCard leuron (unwrapDCard dcard)
    LnDCardX dcardx      -> renderLeuron_DCardX leuron (unwrapDCardX dcardx)
    _                    -> renderLeuron_Unknown leuron
  where
  leuron = ln ^. _LeuronResponse



renderLeuron_Fact :: LeuronResponseR -> FactR -> ComponentHTML Input
renderLeuron_Fact leuron fact =
  H.div_ [
    H.pre_ [H.text fact.text]
  ]




renderLeuron_FactList :: LeuronResponseR -> FactListR -> ComponentHTML Input
renderLeuron_FactList leuron fact_list =
  H.div_ [
    H.pre_ [H.text fact_list.fact],
    H.ul_ $ map (\fact -> H.li_ [H.pre_ [H.text fact]]) fact_list.list
  ]



renderLeuron_Card :: LeuronResponseR -> CardR -> ComponentHTML Input
renderLeuron_Card leuron card =
  H.div_ [
    H.p_ [H.h2_ [H.text "Front"], H.pre_ [H.text card.front]],
    H.p_ [H.h2_ [H.text "Back"], H.pre_ [H.text card.back]]
  ]



renderLeuron_DCard :: LeuronResponseR -> DCardR -> ComponentHTML Input
renderLeuron_DCard leuron dcard =
  H.div_ [
    H.p_ [H.h2_ [H.text "Front"], H.pre_ [H.text dcard.front]],
    H.p_ [H.h2_ [H.text "Back"], H.pre_ [H.text dcard.back]]
  ]



renderLeuron_DCardX :: LeuronResponseR -> DCardXR -> ComponentHTML Input
renderLeuron_DCardX leuron dcardx =
  H.div_ [
    H.p_ [H.h2_ [H.text "Front"],
      H.ul_ $ map (\front -> H.li_ [H.pre_ [H.text front]]) dcardx.front
    ],
    H.p_ [H.h2_ [H.text "Back"],
      H.ul_ $ map (\back -> H.li_ [H.pre_ [H.text back]]) dcardx.back
    ]
  ]



renderLeuron_Unknown :: LeuronResponseR -> ComponentHTML Input
renderLeuron_Unknown leuron =
  H.div_ [H.text "unknown"]
