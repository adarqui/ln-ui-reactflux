{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module LN.UI.Helpers.ReactFluxDOM (
  ahref,
  ahrefName,
  ahrefClasses,
  ahrefClassesName,
  ahrefElement,
  targetValue,
  classNames_,
  className_
) where



import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           React.Flux

import           LN.UI.Helpers.GHCJS (JSString)
import           LN.UI.Router.Route  (Route (..), RouteWith (..),
                                      fromRouteWithHash, linkName)
import           LN.UI.Types         (HTMLEvent_)



ahref :: RouteWith -> HTMLEvent_
ahref route_with = ahrefName (linkName route_with) route_with



ahrefName :: Text -> RouteWith -> HTMLEvent_
ahrefName name route_with =
  a_ ["href" $= fromRouteWithHash route_with] $ elemText name



ahrefClasses :: JSString -> RouteWith -> HTMLEvent_
ahrefClasses classes route_with = ahrefClassesName classes (linkName route_with) route_with



ahrefClassesName :: JSString -> Text -> RouteWith -> HTMLEvent_
ahrefClassesName classes name route_with =
  a_ ["className" $= classes, "href" $= fromRouteWithHash route_with] $ elemText name



-- ahrefElement :: RouteWith -> HTMLEvent_ -> HTMLEvent_
ahrefElement route_with element =
  a_ ["href" $= fromRouteWithHash route_with] element



-- TODO FIXME: Can't use FromJSVal because it's defined in React.Flux.PropertiesAndEvents but not exported
-- -- targetValue :: FromJSVal val => Event -> val
targetValue evt = target evt "value"



classNames_ :: [Text] -> PropertyOrHandler handler
classNames_ = classNames . flip zip (repeat True)



className_ :: JSString -> PropertyOrHandler handler
className_ = ($=) "classNames"
