module LN.View.Users.Profile (
  renderView_Users_Profile
) where



import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, id, ($), (==), (<>))
import Data.Date.Helpers               (yyyy_mm_dd)

import LN.Input.Profile                (InputProfile (..))
import LN.Input.Types                  (Input (..))
import LN.State.Types                  (State)
import LN.T
import LN.View.Users
import LN.View.Util



renderView_Users_Profile :: String -> State -> ComponentHTML Input
renderView_Users_Profile user_nick st =
  usersLayout user_nick st [
    H.div_ [
      showIfSelf st (profile_Self st) (profile_Else st)
    ]
  ]



profile_Else :: State -> ComponentHTML Input
profile_Else st =
  case st.currentUser of
       Nothing -> profile_Else_NotFound st
       Just user -> profile_Else' st user



profile_Else' :: State -> UserSanitizedPackResponse -> ComponentHTML Input
profile_Else' st user =
  H.div [] [
--    H.p_ [H.text $ "gender: " <> (show $ user ^. _UserSanitizedPackResponse .. userProfile_ ^. _ProfileResponse .. gender_)],
    H.p_ [H.text $ "signature: " <> (show $ user ^. _UserSanitizedPackResponse .. userProfile_ ^. _ProfileResponse .. signature_)]
  ]



profile_Else_NotFound :: State -> ComponentHTML Input
profile_Else_NotFound _ = H.div_ [H.text "Not Found"]



profile_Self :: State -> ComponentHTML Input
profile_Self st =
  case st.me of
       Nothing -> profile_Self_NotFound st
       Just user -> profile_Self' st user



profile_Self' :: State -> UserPackResponse -> ComponentHTML Input
profile_Self' st user =
  H.div [] [
    H.h2_ [H.text "profile"],
    H.select [
      P.class_ B.formControl,
      E.onValueChange $ E.input (\v -> CompProfile (InputProfile_Gender v))
    ] [
      H.option [P.selected $ prof.gender == GenderMale]    [H.text "male"],
      H.option [P.selected $ prof.gender == GenderFemale]  [H.text "female"],
      H.option [P.selected $ prof.gender == GenderUnknown] [H.text "unknown"]
    ],
    H.input [
      P.inputType P.InputDate,
      P.classes [B.formControl],
      P.placeholder "birthdate",
      P.value $ yyyy_mm_dd prof.birthdate,
      E.onValueChange $ E.input (\v -> CompProfile (InputProfile_Birthdate v))
    ],
    H.input [
      P.classes [B.formControl], P.placeholder "website",
      P.value $ maybe "" id prof.website,
      E.onValueChange $ E.input (\v -> CompProfile (InputProfile_Website (Just v)))
    ],
    H.input [
      P.classes [B.formControl], P.placeholder "location",
      P.value $ maybe "" id prof.location,
      E.onValueChange $ E.input (\v -> CompProfile (InputProfile_Location (Just v)))
    ],
    H.textarea [
      P.classes [B.formControl], P.placeholder "signature",
      P.value $ maybe "" id prof.signature,
      E.onValueChange $ E.input (\v -> CompProfile (InputProfile_Signature (Just v)))
    ],
    H.button [
      E.onClick $ E.input_ $ CompProfile InputProfile_Post
    ] [H.text "save"]
  ]
  where
  prof = user ^. _UserPackResponse .. userProfile_ ^. _ProfileResponse



profile_Self_NotFound :: State -> ComponentHTML Input
profile_Self_NotFound _ = H.div_ [H.text "Not Found"]
