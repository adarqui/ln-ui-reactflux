module LN.View.Module.LikeThreadPost (
  renderLikeThreadPost
) where



import Data.Maybe                         (Maybe(..), maybe)
import Halogen                            (ComponentHTML)
import Halogen.HTML.Indexed               as H
import Halogen.HTML.Properties.Indexed    as P
import Halogen.HTML.Events.Indexed        as E
import Halogen.Themes.Bootstrap3          as B
import Prelude                            (($))

import LN.Input.LikeThreadPost            (InputLikeThreadPost(..))
import LN.Input.Types                     (Input(..))



renderLikeThreadPost :: Int -> ComponentHTML Input
renderLikeThreadPost thread_post_id =
  H.div_ [H.text "like thread post"]
  {-
  H.div [P.class_ B.row] [
    H.div [P.classes [B.colSmOffset8, B.colSm4]] [
      H.div [P.class_ B.inputGroup] [
        H.input [
          P.class_ B.formControl,
          P.placeholder "title for new thread",
          P.value name,
          E.onValueChange $ E.input (\v -> CompCreateThread $ InputCreateThread_SetName (Just v))
        ],
        H.span [P.class_ B.inputGroupBtn] [
          H.button [
            P.classes [B.btn, B.btnDefault],
            E.onClick $ E.input_ $ CompCreateThread InputCreateThread_Create
          ] [H.text "create"]
        ]
      ]
    ]
  ]
  where
  name = maybe "" (\comp -> comp.name) mcomp
  -}
