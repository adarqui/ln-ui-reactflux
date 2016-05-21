module LN.View.Resources.SiftLeurons.Show (
  renderView_Resources_SiftLeurons_Show
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H
import Prelude              (($), (<>))

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Resources_SiftLeurons_Show :: Int -> String -> State -> ComponentHTML Input
renderView_Resources_SiftLeurons_Show resource_id s_sift _ = H.div_ [H.text $ "resources sift leurons show: " <> s_sift ]

-- parse s_sift for linear, rand, ... and who knows what else??
-- IDEA:
-- /resources/1/sift/linear?offset=1 ?offset=2
-- /resources/1/sift/random?leuron_types=[fact,card] ??
