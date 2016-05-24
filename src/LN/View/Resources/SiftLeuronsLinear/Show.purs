module LN.View.Resources.SiftLeuronsLinear.Show (
  renderView_Resources_SiftLeuronsLinear_Show
) where



import Data.Int                        (fromString)
import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))

import LN.Input.Types                  (Input)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Link                  (linkToP_Classes)
import LN.State.Types                  (State)
import LN.View.Leurons.Show            (renderView_Leurons_Show')
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( LeuronPackResponse
                                       , _LeuronPackResponse, _LeuronResponse
                                       , leuron_)



renderView_Resources_SiftLeuronsLinear_Show :: Int -> String -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Show resource_id s_offset st =
  case st.currentLeuron, fromString s_offset of
       _, Nothing             -> H.div_ [H.p_ [H.text "Invalid offset"]]
       Nothing, _             -> renderLoading
       Just pack, Just offset -> renderView_Resources_SiftLeuronsLinear_Show' pack offset st



renderView_Resources_SiftLeuronsLinear_Show' :: LeuronPackResponse -> Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Show' pack offset st =
  H.div_ [
    renderButtons pack st,
    renderView_Leurons_Show' pack st,
    renderButtons pack st
  ]



renderButtons :: LeuronPackResponse -> State -> ComponentHTML Input
renderButtons pack st =
  H.div_ [
    H.div [P.class_ B.listGroup] [
      linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeuronsLinear leuron.resourceId Index []) "prev",
      linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeuronsLinear leuron.resourceId Index []) "next"
    ]
  ]
  where
  leuron = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
