{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.DOM (
  ahref
) where



import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           LN.UI.Router.Class.Routes2
import           React.Flux



ahref :: RoutesWith -> ReactElementM eventHandler ()
ahref route_with =
  a_ ["href" $= "#"] $ elemText name
  where
  name = linkName route_with
