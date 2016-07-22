{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.DOM (
  ahref,
  ahrefName
) where



import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           LN.UI.Router.Class.Route
import           React.Flux



ahref :: RouteWith -> ReactElementM eventHandler ()
ahref route_with = ahrefName (linkName route_with) route_with



ahrefName :: Text -> RouteWith -> ReactElementM eventHandler ()
ahrefName name route =
  a_ ["href" $= fromRouteWithHash route] $ elemText name
