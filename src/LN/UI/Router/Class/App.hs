{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.Class.App (
  RoutingApp (..)
) where



import           Control.Applicative ((<|>))
import           Control.DeepSeq     (NFData)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)
import           Web.Routes



data RoutingApp
  = AppHome
  | AppAbout
  deriving (Show, Generic, Typeable, NFData)



instance PathInfo RoutingApp where
  toPathSegments route =
    case route of
      AppHome  -> pure "#/"
      AppAbout -> pure "#/about"
  fromPathSegments =
        AppHome  <$ segment "#/"
    <|> AppAbout <$ segment "#/about"
    <|> pure AppHome
