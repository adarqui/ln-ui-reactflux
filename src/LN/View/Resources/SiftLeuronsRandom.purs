module LN.View.Resources.SiftLeuronsRandom (
  renderView_Resources_SiftLeuronsRandom
) where



import Data.Maybe           (Maybe(..))
import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.Router.Types      (Routes(..))
import LN.Router.Link       (linkTo)
import LN.State.Types       (State)
import LN.View.Leurons.Show (renderView_Leurons_Show')
import LN.T                 (LeuronPackResponse)



renderView_Resources_SiftLeuronsRandom :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom resource_id st =
  case st.currentLeuron of
    Nothing   -> H.div_ [H.p_ [H.text "what?"]]
    Just pack -> renderView_Resources_SiftLeuronsRandom' pack st



renderView_Resources_SiftLeuronsRandom' :: LeuronPackResponse -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom' pack st =
  renderView_Leurons_Show' pack st
