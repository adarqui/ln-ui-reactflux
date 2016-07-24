{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Field (
  mandatoryNameField
) where



import           React.Flux          hiding (view)
import qualified React.Flux          as RF
import           React.Flux.Internal

import           LN.UI.Helpers.GHCJS (JSString)



mandatoryLabelField :: JSString -> JSString -> (Event -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryLabelField label value handler =
  createLabelInput label label value $ onChange handler



mandatoryNameField :: JSString -> (Event -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryNameField = mandatoryLabelField "Name"


mandatoryCompanyField :: JSString -> (Event -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryCompanyField = mandatoryLabelField "Company"


mandatoryLocationField :: JSString -> (Event -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryLocationField = mandatoryLabelField "Location"



createLabelInput :: JSString -> JSString -> JSString -> PropertyOrHandler ViewEventHandler -> ReactElementM ViewEventHandler ()
createLabelInput label placeholder value handler =
  cldiv_ "labeled-input" $ do
    label_ [] $ elemJSString label
    input_ [ "className"   $= "todo-fixme"
           , "type"        $= "text"
           , "value"       $= value
           , "placeholder" $= placeholder
           , handler
           ]
  -- H.div
  --   [formGroupClasses]
  --   [
  --     H.label_ [H.text label],
  --     H.input [
  --       formControlClasses,
  --       P.inputType input_type,
  --       P.placeholder placeholder,
  --       P.value value,
  --       E.onValueChange handler
  --     ]
  --   ]
