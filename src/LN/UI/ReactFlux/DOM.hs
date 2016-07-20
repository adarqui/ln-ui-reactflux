{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.DOM (
  ahref,
  ahrefName
) where



import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           LN.UI.Router.Class.Routes2
import           React.Flux



ahref :: RoutesWith -> ReactElementM eventHandler ()
ahref route_with = ahrefName (linkName route_with) route_with



ahrefName :: Text -> RoutesWith -> ReactElementM eventHandler ()
ahrefName name route_with =
  a_ ["href" $= "#"] $ elemText name
