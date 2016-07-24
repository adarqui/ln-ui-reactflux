{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Field (
  mandatoryLabelField,
  mandatoryNameField,
  mandatoryCompanyField,
  mandatoryLocationField,
  optionalDescriptionField,
  mandatoryDescriptionField,
  mandatoryMembershipField,
  mandatoryVisibilityField
) where



import           Data.Text                  (Text)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF
import           React.Flux.Internal

import           LN.UI.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.Helpers.ReactFluxDOM (targetValue)
import           LN.UI.View.Internal
import LN.T.Membership (Membership(..))
import LN.T.Visibility (Visibility(..))



mandatoryLabelField :: Text -> Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryLabelField label value handler =
  createLabelInput label label value $ onChange (handler . targetValue)



mandatoryNameField :: Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryNameField = mandatoryLabelField "Name"


mandatoryCompanyField :: Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryCompanyField = mandatoryLabelField "Company"


mandatoryLocationField :: Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryLocationField = mandatoryLabelField "Location"



optionalDescriptionField :: Maybe Text -> (Text -> ViewEventHandler) -> ViewEventHandler -> ReactElementM ViewEventHandler ()
optionalDescriptionField m_value set_description_handler remove_description_handler =
  createLabelButtonTextArea "Description" "Description" (maybe "" id m_value) "✖"
    (onChange (set_description_handler . targetValue))
    (onClick $ const . const remove_description_handler)



mandatoryDescriptionField :: Text -> (Text -> ViewEventHandler) -> ViewEventHandler -> ReactElementM ViewEventHandler ()
mandatoryDescriptionField value set_description_handler remove_description_handler =
  createLabelButtonTextArea "Description" "Description" value "✖"
    (onChange (set_description_handler . targetValue))
    (onClick $ const . const remove_description_handler)



mandatoryMembershipField :: Membership -> (Membership -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryMembershipField value membership_handler =
  createRadioMenu
    "Membership"
    "membership"
    [ Membership_InviteOnly
    , Membership_RequestInvite
    , Membership_Join
    , Membership_Locked
    ]
    value
    (onChange $ membership_handler . read . targetValue)



mandatoryVisibilityField :: Visibility -> (Visibility -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryVisibilityField value visibility_handler =
  createRadioMenu
    "Visibility"
    "visibility"
    [Public, Private]
    value
    (onChange $ visibility_handler . read . targetValue)
