module LN.View.Resources.SiftLeuronsRandom (
  renderView_Resources_SiftLeuronsRandom
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed     as E
import Optic.Core                      ((^.), (..))
import Prelude                         (($))

import LN.Input.Types                  (Input(..))
import LN.Router.Types                 (Routes(..))
import LN.Router.Link                  (linkTo)
import LN.State.Types                  (State)
import LN.View.Leurons.Show            (renderView_Leurons_Show')
import LN.T                            (LeuronPackResponse
                                       , _LeuronPackResponse, _LeuronResponse
                                       , leuron_)



renderView_Resources_SiftLeuronsRandom :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom resource_id st =
  case st.currentLeuron of
    Nothing   -> H.div_ [H.p_ [H.text "what?"]]
    Just pack -> renderView_Resources_SiftLeuronsRandom' pack st



renderView_Resources_SiftLeuronsRandom' :: LeuronPackResponse -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom' pack st =
  H.div_ [
    renderKnowledgeButtons pack st,
    renderView_Leurons_Show' pack st,
    renderKnowledgeButtons pack st
  ]



renderKnowledgeButtons :: LeuronPackResponse -> State -> ComponentHTML Input
renderKnowledgeButtons pack st =
  H.div_ [
    randomLeuronButton pack
  ]



randomLeuronButton :: LeuronPackResponse -> ComponentHTML Input
randomLeuronButton pack =
  H.div_ [
    H.button [
      E.onClick $ E.input_ $ GetResourceLeuronRandom resource.resourceId
    ] [H.text "next"]
  ]
  where
  resource = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
