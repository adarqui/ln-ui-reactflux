module LN.View.Resources.SiftLeuronsLinear.Show (
  renderView_Resources_SiftLeuronsLinear_Show
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         ((==), (+), (-))

import LN.Input.Types                  (Input)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentLeuron)
import LN.State.Types                  (State)
import LN.View.Leurons.Show            (renderView_Leurons_Show')
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( LeuronPackResponse
                                       , _LeuronPackResponse, _LeuronResponse
                                       , leuron_)



renderView_Resources_SiftLeuronsLinear_Show :: Int -> Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Show resource_id offset st =
  case st.currentLeuron, getLoading l_currentLeuron st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "no more leurons"]]
       Just pack, false -> renderView_Resources_SiftLeuronsLinear_Show' pack offset st



renderView_Resources_SiftLeuronsLinear_Show' :: LeuronPackResponse -> Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Show' pack offset st =
  H.div_ [
    renderButtons pack offset st,
    renderView_Leurons_Show' pack st,
    renderButtons pack offset st
  ]



renderButtons :: LeuronPackResponse -> Int -> State -> ComponentHTML Input
renderButtons pack offset st =
  H.div [P.class_ B.row] [
    H.div [P.classes [B.colLg2, B.colMd2, B.colXs6]] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (ResourcesSiftLeuronsLinear leuron.resourceId (ShowI offset_prev) emptyParams) "prev"
    ],
    H.div [P.classes [B.colLg2, B.colMd2, B.colXs6]] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (ResourcesSiftLeuronsLinear leuron.resourceId (ShowI offset_next) emptyParams) "next"
    ]
  ]
  where
  leuron      = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
  offset_prev = if offset == 0 then 0 else (offset-1)
  offset_next = offset+1
