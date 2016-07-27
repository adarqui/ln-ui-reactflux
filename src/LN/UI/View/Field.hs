{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Field (
  mandatoryLabelField,
  mandatoryNameField,
  mandatoryCompanyField,
  mandatoryLocationField,
  optionalDescriptionField,
  mandatoryDescriptionField,
  mandatoryMembershipField,
  mandatoryVisibilityField,
  tagsField,
  privateTagsField,
  suggestedTagsField,
  mandatoryIntegerField
) where



import           Data.Text                  (Text)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF
import           React.Flux.Internal
import qualified Web.Bootstrap3 as B

import           LN.T.Membership            (Membership (..))
import           LN.T.Visibility            (Visibility (..))
import           LN.UI.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.Helpers.ReactFluxDOM (targetValue, className_, classNames_)
import           LN.UI.Types                (HTMLView_)
import           LN.UI.View.Internal



mandatoryLabelField :: Text -> Text -> (Text -> ViewEventHandler) -> HTMLView_
mandatoryLabelField label value handler =
  createLabelInput label label value $ onChange (handler . targetValue)



mandatoryNameField :: Text -> (Text -> ViewEventHandler) -> HTMLView_
mandatoryNameField = mandatoryLabelField "Name"


mandatoryCompanyField :: Text -> (Text -> ViewEventHandler) -> HTMLView_
mandatoryCompanyField = mandatoryLabelField "Company"


mandatoryLocationField :: Text -> (Text -> ViewEventHandler) -> HTMLView_
mandatoryLocationField = mandatoryLabelField "Location"



optionalDescriptionField :: Maybe Text -> (Text -> ViewEventHandler) -> ViewEventHandler -> HTMLView_
optionalDescriptionField m_value set_description_handler remove_description_handler =
  createLabelButtonTextArea "Description" "Description" (maybe "" id m_value) "✖"
    (onChange (set_description_handler . targetValue))
    (onClick $ const . const remove_description_handler)



mandatoryDescriptionField :: Text -> (Text -> ViewEventHandler) -> ViewEventHandler -> HTMLView_
mandatoryDescriptionField value set_description_handler remove_description_handler =
  createLabelButtonTextArea "Description" "Description" value "✖"
    (onChange (set_description_handler . targetValue))
    (onClick $ const . const remove_description_handler)



mandatoryMembershipField :: Membership -> (Membership -> ViewEventHandler) -> HTMLView_
mandatoryMembershipField value membership_handler =
  createRadioMenu
    "Membership"
    "membership"
    [Membership_InviteOnly, Membership_RequestInvite, Membership_Join, Membership_Locked]
    value
    membership_handler



mandatoryVisibilityField :: Visibility -> (Visibility -> ViewEventHandler) -> HTMLView_
mandatoryVisibilityField value visibility_handler =
  createRadioMenu
    "Visibility"
    "visibility"
    [Public, Private]
    value
    visibility_handler



tagsField
  :: [Text]
  -> Text
  -> (Text -> ViewEventHandler) -- ^ set current tag
  -> ViewEventHandler           -- ^ add current tag
  -> (Int -> ViewEventHandler)  -- ^ delete tag
  -> ViewEventHandler           -- ^ clear tags
  -> HTMLView_
tagsField = createTagsField "Tags"



privateTagsField
  :: [Text]
  -> Text
  -> (Text -> ViewEventHandler) -- ^ set current tag
  -> ViewEventHandler           -- ^ add current tag
  -> (Int -> ViewEventHandler)  -- ^ delete tag
  -> ViewEventHandler           -- ^ clear tags
  -> HTMLView_
privateTagsField = createTagsField "Private Tags"



suggestedTagsField
  :: [Text]
  -> Text
  -> (Text -> ViewEventHandler) -- ^ set current tag
  -> ViewEventHandler           -- ^ add current tag
  -> (Int -> ViewEventHandler)  -- ^ delete tag
  -> ViewEventHandler           -- ^ clear tags
  -> HTMLView_
suggestedTagsField = createTagsField "Suggested thread post tags - so people can easily tag posts"



mandatoryIntegerField
  :: Text -- ^ label
  -> Int  -- ^ value
  -> Int  -- ^ default value
  -> Int  -- ^ minimum value
  -> Int  -- ^ maximum value
  -> Int  -- ^ step count
  -> (Int -> ViewEventHandler) -- ^ set value handler
  -> HTMLView_

mandatoryIntegerField label value default_value min max step set_value_handler = do
  div_ $ do
    cldiv_ B.inputGroup $ do
      label_ $ elemText label
      input_ [ classNames_ formControlClasses
             , "value" @= value
             , "type"  $= "number"
             , "min"   @= min
             , "max"   @= max
             , "step"  @= step
             , onChange (set_value_handler . read . targetValue)
             ]
      span_ [className_ B.inputGroupBtn] $ do
        button_ [ classNames_ buttonInfoClasses
                , "title" $= "Default"
                , onClick $ \_ _ -> set_value_handler default_value
                ] $ elemText "Default"
