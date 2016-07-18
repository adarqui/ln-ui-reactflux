{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.Class.App (
  RoutingApp (..)
) where



import Control.Applicative ((<|>))
import           Web.Routes
import GHC.Generics (Generic)
import Data.Typeable (Typeable)



data RoutingApp
  = AppHome
  | AppAbout
  deriving (Show, Generic, Typeable)



instance PathInfo RoutingApp where
  toPathSegments route =
    case route of
      AppHome  -> pure "#/"
      AppAbout -> pure "#/about"
  fromPathSegments =
        AppHome  <$ segment "#/"
    <|> AppAbout <$ segment "#/about"
    <|> pure AppHome
