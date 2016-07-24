{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Internal (
  buttonInfoClasses,
  buttonDangerClasses,
  buttonClasses,
  formGroupClasses,
  formControlClasses,
  radioInlineClasses,
  createLabelInput,
  createLabelButtonTextArea,
  createRadioMenu
) where


import           Data.Text                  (Text)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF
import           React.Flux.Internal

import           LN.UI.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.Helpers.ReactFluxDOM (targetValue)



--
-- Buttons
--

buttonInfoClasses :: [(Text, Bool)]
buttonInfoClasses = buttonClasses "btn-info"

buttonDangerClasses :: [(Text, Bool)]
buttonDangerClasses = buttonClasses "btn-danger"

buttonClasses :: Text -> [(Text, Bool)]
buttonClasses button_type =  [("btn", True), (button_type, True)]



--
-- Forms
--

formGroupClasses :: [(Text, Bool)]
formGroupClasses = [("form-group", True)]

formControlClasses :: [(Text, Bool)]
formControlClasses = [("form-control", True)]



--
-- Radio
--

radioInlineClasses :: [(Text, Bool)]
radioInlineClasses = [("radio-inline", True)]



--
-- Widgets
--

createLabelInput :: Text -> Text -> Text -> PropertyOrHandler ViewEventHandler -> ReactElementM ViewEventHandler ()
createLabelInput label placeholder value handler =
  div_ [ classNames formGroupClasses
       ] $ do
    label_ [] $ elemText label
    input_ [ classNames formControlClasses
           , "type"        $= "text"
           , "placeholder" $= textToJSString' placeholder
           , "value"       $= textToJSString' value
           , handler
           ]



createLabelButtonTextArea
  :: Text
  -> Text
  -> Text
  -> Text
  -> PropertyOrHandler ViewEventHandler
  -> PropertyOrHandler ViewEventHandler
  -> ReactElementM ViewEventHandler ()

createLabelButtonTextArea label placeholder value button_name value_change_handler button_handler =
  cldiv_ "label-button-textarea input-group" $ do
    label_ [] $ elemText label
    textarea_ [ classNames formControlClasses
              , "placeholder" $= textToJSString' placeholder
              , "value"       $= textToJSString' value
              , value_change_handler
              ] mempty
    span_ [ "className" $= "input-group-btn" ] $ do
      button_ [ classNames buttonInfoClasses
              , "title" $= textToJSString' button_name
              , button_handler
              ] $ elemText button_name



-- | Creates a radio menu
--
-- radioMenu "Leuron Type" "leuron-type" [NONE, FACT, CARD] SetLeuronType
--
createRadioMenu
  :: forall a radio. (Show radio, Eq radio)
  => Text
  -> Text
  -> [radio]
  -> radio
  -> (radio -> ViewEventHandler)
  -> ReactElementM ViewEventHandler ()

createRadioMenu menu_label radio_name radios checked_value radio_handler =
  p_ $ do
    label_ $ elemText menu_label
    mapM_ (\radio -> do
      label_ [ classNames radioInlineClasses ] $ elemShow radio
      input_ [ "type"    $= "radio"
             , "name"    $= textToJSString' radio_name
             , "checked" @= (checked_value == radio)
             , onChange (\_ -> radio_handler radio)
             ]
      ) radios
