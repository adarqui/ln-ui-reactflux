module LN.View.Errors (
  renderView_Errors
) where



import Data.Tuple           (Tuple(..))
import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H
import Prelude              (map, ($), (<>))

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Errors :: State -> ComponentHTML Input
renderView_Errors st =
  H.div_ [
    H.p_ [
      H.h1_ [ H.text "Errors"],
      H.ul_ $ map (\(Tuple author err) -> H.li_ [H.text $ author <> " -> " <> err]) st.errors
    ]
 ]
