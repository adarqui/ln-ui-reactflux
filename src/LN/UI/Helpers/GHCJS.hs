{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Helpers.GHCJS (
  JSString,
  textToJSString',
  toJSString'
) where



import           Data.Text           (Text)
import qualified Data.Text           as Text

import           React.Flux
import           React.Flux.Internal (JSString)

#ifdef __GHCJS__
import qualified Data.JSString.Text  as JSS (textToJSString)
import qualified GHCJS.Foreign       as Foreign (toJSString)
#endif



#ifdef __GHCJS__
textToJSString' :: Text -> JSString
textToJSString' = JSS.textToJSString
#else
textToJSString' :: Text -> String
textToJSString' = Text.unpack
#endif



#ifdef __GHCJS__
toJSString' :: Show a => a -> JSString
toJSString' = Foreign.toJSString . show
#else
toJSString' :: Show a => a -> String
toJSString' = show
#endif
