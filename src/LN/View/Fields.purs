module LN.View.Fields (
  mandatoryLabelField,
  mandatoryNameField,
  mandatoryCompanyField,
  mandatoryLocationField,
  optionalDescriptionField,
  mandatoryDescriptionField,
  mandatoryMembershipField,
  mandatoryVisibilityField
) where



import Data.Maybe                      (Maybe(..), maybe)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events.Indexed     as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, show, ($))

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
