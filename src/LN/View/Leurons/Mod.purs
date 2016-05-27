module LN.View.Leurons.Mod (
  renderView_Leurons_New,
  renderView_Leurons_Edit,
  renderView_Leurons_Mod
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
-- import LN.Internal.Leuron
import LN.Input.Leuron
import LN.Input.Types                  (Input(..), cLeuronMod)
import LN.State.Loading                (getLoading, l_currentLeuron)
import LN.State.Leuron                 (LeuronRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Leurons_New :: State -> ComponentHTML Input
renderView_Leurons_New = renderView_Leurons_Mod Nothing



renderView_Leurons_Edit :: Int -> State -> ComponentHTML Input
renderView_Leurons_Edit leuron_id = renderView_Leurons_Mod (Just leuron_id)



renderView_Leurons_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Leurons_Mod m_leuron_id st =
  case st.currentLeuronRequest, st.currentLeuronRequestSt, getLoading l_currentLeuron st.loading of
    _, _, true                         -> renderLoading
    Just leuron_req, Just rst, false -> renderView_Leurons_Mod' m_leuron_id leuron_req rst st
    _, _, false                        -> H.div_ [H.p_ [H.text "unexpected error."]]



renderView_Leurons_Mod' :: Maybe Int -> LeuronRequest -> LeuronRequestState -> State -> ComponentHTML Input
renderView_Leurons_Mod' m_leuron_id leuron_req lst st =
  H.div_ [
    H.h1_
      [ H.text "Add Leuron" ]
  ]
