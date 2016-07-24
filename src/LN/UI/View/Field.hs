{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Field (
  mandatoryLabelField,
  mandatoryNameField,
  mandatoryCompanyField,
  mandatoryLocationField
) where


import           Data.Text                  (Text)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF
import           React.Flux.Internal

import           LN.UI.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.Helpers.ReactFluxDOM (targetValue)



mandatoryLabelField :: Text -> Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryLabelField label value handler =
  createLabelInput label label value $ onChange (handler . targetValue)



mandatoryNameField :: Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryNameField = mandatoryLabelField "Name"


mandatoryCompanyField :: Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryCompanyField = mandatoryLabelField "Company"


mandatoryLocationField :: Text -> (Text -> ViewEventHandler) -> ReactElementM ViewEventHandler ()
mandatoryLocationField = mandatoryLabelField "Location"



createLabelInput :: Text -> Text -> Text -> PropertyOrHandler ViewEventHandler -> ReactElementM ViewEventHandler ()
createLabelInput label placeholder value handler =
  cldiv_ "labeled-input" $ do
    label_ [] $ elemText label
    input_ [ "className"   $= "todo-fixme"
           , "type"        $= "text"
           , "value"       $= textToJSString' value
           , "placeholder" $= textToJSString' placeholder
           , handler
           ]
