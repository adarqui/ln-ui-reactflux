{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Internal (
  buttonInfoClasses,
  buttonDangerClasses,
  buttonClasses,
  formGroupClasses,
  formControlClasses,
  radioInlineClasses,
  createLabelInput,
  createLabelButtonTextArea
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
  cldiv_ "labeled-input" $ do
    label_ [] $ elemText label
    input_ [ "className"   $= "todo-fixme"
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
