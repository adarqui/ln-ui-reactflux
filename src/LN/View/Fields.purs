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
  privateTagsField,
  suggestedTagsField,
  mandatoryIntegerField,
  mandatoryBooleanField,
  mandatoryBooleanYesNoField
) where



import Data.Array                      (concat)
import Data.Int                        (fromString)
import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen.HTML.Core               as C
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events.Indexed     as E
import Halogen.HTML.Properties         as Pr
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, show, map, ($), (==), (<<<))

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



-- | Generic "Tags" function
-- label: Tags, Private Tags
-- tags: Array of tag strings
-- current_tag: current string which may be added as a tag
-- set_tag: set st.current_tag, which is what we have been editing
-- add_tag: add tag to st.tags
-- delete_tag: delete tag by index
-- clear_tags: remove all tags
internalTagsField label tags current_tag set_tag add_tag delete_tag clear_tags =
  H.div_ [
    H.div [P.class_ B.inputGroup] [
      H.label_ [H.text label],
      H.input [
        formControlClasses,
        P.value current_tag,
        P.inputType P.InputText,
        E.onValueChange $ E.input set_tag,
        E.onKeyUp $ E.input (\ev -> if ev.keyCode == 13.0 then add_tag else Nop)
      ],
      H.span [P.class_ B.inputGroupBtn] [
        H.button [
          buttonInfoClasses,
          P.title "Add",
          E.onClick $ E.input_ add_tag
        ] [H.text "Add"]
      ]
    ],
    H.div_ $ concat $ map (\(Tuple idx tag) ->
        [
        H.span [P.classes [B.label, B.labelDefault]] [
          H.text tag
        ],
        H.span [] [
          H.button [
            P.classes [B.btn, B.btnDefault, B.btnXs],
            E.onClick $ E.input_ $ delete_tag idx
          ] [H.span [P.classes [B.glyphicon, B.glyphiconRemove]] []]
        ],
        H.text " "
        ]
      ) $ seqArrayFrom tags
  ]

tagsField = internalTagsField "Tags"

privateTagsField = internalTagsField "Private Tags"

suggestedTagsField = internalTagsField "Suggested thread post tags - so people can easily tag posts"

mandatoryIntegerField label value default min max step set_cb =
  H.div_ [
    H.div [P.class_ B.inputGroup] [
      H.label_ [H.text label],
      H.input [
        formControlClasses,
        P.value $ show value,
        P.inputType P.InputNumber,
--        P.min min,
--        P.max max,
--        P.step step,
        E.onValueChange $ E.input (set_cb <<< maybe default id <<< fromString)
      ],
      H.span [P.class_ B.inputGroupBtn] [
        H.button [
          buttonInfoClasses,
          P.title "Default",
          E.onClick $ E.input_ $ set_cb default
        ] [H.text "Default"]
      ]
    ]
  ]



boolFromString :: String -> Boolean
boolFromString "true" = true
boolFromString _      = false



boolFromYesNoString :: String -> Boolean
boolFromYesNoString "true" = true
boolFromYesNoString _      = false



mandatoryBooleanField label value default set_cb =
  internalSelectList label value default [true, false] (set_cb)



mandatoryBooleanYesNoField label value default set_cb =
  internalSelectList label value default [true, false] (set_cb)
--  internalSelectList label (show value) (show default) ["yes", "no"] (set_cb <<< boolFromYesNoString)



internalSelectList label value default options set_cb =
  H.div_ [
    H.div [P.class_ B.inputGroup] [
      H.label_ [H.text label],
      H.select [
         P.class_ B.formControl,
          E.onValueChange $ E.input (set_cb <<< boolFromString)
        ] $
        map (\option -> H.option [P.selected $ value == option] [H.text $ show option]) options,
      H.span [P.class_ B.inputGroupBtn] [
        H.button [
          buttonInfoClasses,
          P.title "Default",
          E.onClick $ E.input_ $ set_cb default
        ] [H.text "Default"]
      ]
    ]
  ]
