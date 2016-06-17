module LN.View.Fields (
  mandatoryLabelField,
  mandatoryNameField,
  mandatoryCompanyField,
  mandatoryLocationField,
  optionalDescriptionField,
  mandatoryDescriptionField,
  mandatoryMembershipField,
  mandatoryVisibilityField,
  internalTagsField,
  tagsField,
  privateTagsField
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events.Indexed     as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, show, map, ($))

import LN.Helpers.Array                (seqArrayFrom)
import LN.Halogen.Util
import LN.Input.Types                  (Input(..))
import LN.Router.Link                  (linkToHref)
import LN.T                            (Membership(..), Visibility(..))



mandatoryLabelField label value set_name =
  input_Label label label value P.InputText (E.input set_name)

mandatoryNameField = mandatoryLabelField "Name"

mandatoryCompanyField = mandatoryLabelField "Company"

mandatoryLocationField = mandatoryLabelField "Location"



-- |
--
optionalDescriptionField m_value set_description remove_description =
  textArea_LabelWithButton "Description" "Description" (maybe "" id m_value) "✖"
    (E.input set_description)
    (E.input_ remove_description)




mandatoryDescriptionField value set_description remove_description =
  textArea_LabelWithButton "Description" "Description" value "✖"
    (E.input set_description)
    (E.input_ remove_description)



mandatoryMembershipField value set_membership =
 radioMenu
  "Membership"
  "membership"
  [ Membership_InviteOnly
  , Membership_RequestInvite
  , Membership_Join
  , Membership_Locked
  ]
  set_membership
  value



mandatoryVisibilityField value set_visibility =
 radioMenu
  "Visibility"
  "visibility"
  [Public, Private]
  set_visibility
  value



internalTagsField label tags current_tag set_tag add_tag delete_tag clear_tags =
  H.div_ [
    H.label_ [H.text label],
    H.input [P.value current_tag, P.inputType P.InputText, E.onValueChange $ E.input set_tag],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        buttonInfoClasses,
        P.title "Add",
        E.onClick $ E.input_ add_tag
      ] [H.text "Add"]
    ],
    H.div_ $ map (\(Tuple idx tag) ->
        H.p_ [
          H.text tag,
          H.span [] [
            H.button [
              E.onClick $ E.input_ $ delete_tag idx
            ] [H.text "x"]
          ]
        ]
      ) $ seqArrayFrom tags
  ]

tagsField = internalTagsField "Tags"

privateTagsField = internalTagsField "Private Tags"
