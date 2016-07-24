{-# LANGUAGE PartialTypeSignatures #-}

module LN.UI.View.Field (
  mandatoryNameField
) where



import           React.Flux          hiding (view)
import qualified React.Flux          as RF

import           LN.UI.Helpers.GHCJS (JSString)



mandatoryLabelField :: JSString -> JSString -> (Event -> handler) -> ReactElementM ViewEventHandler ()
mandatoryLabelField label value handler =
  createLabelInput label label value $ onChange handler



mandatoryNameField :: JSString -> (Event -> handler) -> ReactElementM ViewEventHandler ()
mandatoryNameField = mandatoryLabelField "Name"


mandatoryCompanyField :: JSString -> (Event -> handler) -> ReactElementM ViewEventHandler ()
mandatoryCompanyField = mandatoryLabelField "Company"


mandatoryLocationField :: JSString -> (Event -> handler) -> ReactElementM ViewEventHandler ()
mandatoryLocationField = mandatoryLabelField "Location"



createLabelInput :: JSString -> JSString -> JSString -> PropertyOrHandler handler -> ReactElementM ViewEventHandler ()
createLabelInput label placeholder value handler =  mempty
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
