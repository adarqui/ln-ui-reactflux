{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Breadcrumbs (
  view
) where



import           Control.Monad                        (forM_)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.UI.Core.Router
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, className_)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)



view :: RouteWith -> HTMLView_
view !route_with' = do
  defineViewWithSKey "breadcrumbs" route_with' go
  where
  go :: RouteWith -> HTMLView_
  go (RouteWith route params) = do
    case (crumb route) of
      [] -> mempty
      xs -> do
        div_ $ do
          ol_ [className_ B.breadcrumb] $
            forM_ (zip [(1 :: Int)..] xs) $ \(idx, breadcrumb) -> li_ ["key" @= idx] $ ahref $ routeWith' breadcrumb
