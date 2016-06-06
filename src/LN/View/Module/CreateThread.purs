module LN.View.Module.CreateThread where
--  renderCreateThread



import Data.Maybe                         (Maybe(..), maybe)
import Halogen                            (ComponentHTML)
import Halogen.HTML.Indexed               as H
import Halogen.HTML.Properties.Indexed    as P
import Halogen.HTML.Events.Indexed        as E
import Halogen.Themes.Bootstrap3          as B
import Prelude                            (($))

-- import LN.Component.CreateThread          (Comp_CreateThread_State)
-- import LN.Input.CreateThread              (InputCreateThread(..))
import LN.Input.Types                     (Input(..))



{-
renderCreateThread :: Maybe Comp_CreateThread_State -> ComponentHTML Input
renderCreateThread mcomp =
  H.div [P.class_ B.row] [
    H.div [P.classes [B.colXsOffset8, B.colXs4]] [
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
