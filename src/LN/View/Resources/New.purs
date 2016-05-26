module LN.View.Resources.New (
  renderView_Resources_New
) where



import Data.Maybe            (Maybe(Nothing))
import Halogen               (ComponentHTML)

import LN.Input.Types        (Input)
import LN.State.Types        (State)
import LN.View.Resources.Mod (renderView_Resources_Mod)



renderView_Resources_New :: State -> ComponentHTML Input
renderView_Resources_New = renderView_Resources_Mod Nothing
