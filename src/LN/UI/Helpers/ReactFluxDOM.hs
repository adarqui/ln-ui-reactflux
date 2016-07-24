{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Helpers.ReactFluxDOM (
  ahref,
  ahrefName,
  ahrefElement,
  targetValue
) where



import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           LN.UI.Router.Route
import           React.Flux



ahref :: RouteWith -> ReactElementM eventHandler ()
ahref route_with = ahrefName (linkName route_with) route_with



ahrefName :: Text -> RouteWith -> ReactElementM eventHandler ()
ahrefName name route_with =
  a_ ["href" $= fromRouteWithHash route_with] $ elemText name



-- ahrefElement :: RouteWith -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
ahrefElement route_with element =
  a_ ["href" $= fromRouteWithHash route_with] element



-- TODO FIXME: Can't use FromJSVal because it's defined in React.Flux.PropertiesAndEvents but not exported
-- -- targetValue :: FromJSVal val => Event -> val
targetValue evt = target evt "value"
