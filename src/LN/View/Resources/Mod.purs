module LN.View.Resources.Mod (
  renderView_Resources_Mod
) where



import Data.Maybe           (Maybe(..))
import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Resources_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Resources_Mod m_resource_id st =
  case m_resource_id, st.currentResourceRequest of
       Just resource_id, Nothing -> H.div_ [H.text "Resource unavailable."]
       _, _                      -> renderView_Resources_Mod' m_resource_id st



renderView_Resources_Mod' :: Maybe Int -> State -> ComponentHTML Input
renderView_Resources_Mod' m_resource_id st =
  H.div_ [H.text "boop"]
