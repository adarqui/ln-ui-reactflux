{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Helpers.GHCJS (
  JSString,
  textToJSString'
) where



import           Data.Text           (Text)
import qualified Data.Text           as Text

import           React.Flux.Internal (JSString)

#ifdef __GHCJS__
import qualified Data.JSString.Text  as JSS (textToJSString)
#endif



#ifdef __GHCJS__
textToJSString' :: Text -> JSString
textToJSString' = JSS.textToJSString
#else
textToJSString' :: Text -> String
textToJSString' = Text.unpack
#endif
