{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans  #-}

module LN.UI.ReactFlux.App.Layout (
  view
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (void)
import           Control.Monad.IO.Class                (liftIO)
import           Data.List                             ((\\))
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import           React.Flux.Router.WebRoutes           (initRouterRaw'ByteString)
import qualified Web.Bootstrap3                        as B

import           LN.T.User                             (UserResponse (..))
import           LN.UI.Core.Control                    (CoreResult (..))
import qualified LN.UI.Core.Loader                     as Loader (loader1)
import           LN.UI.Core.PageInfo                   (PageInfo,
                                                        defaultPageInfo)
import           LN.UI.Core.Router
import           LN.UI.Core.State                      (Action (..), Store (..),
                                                        defaultStore)
import qualified LN.UI.ReactFlux.App.Breadcrumbs       as Breadcrumbs
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Errors            as Errors
import           LN.UI.ReactFlux.App.Loader
import qualified LN.UI.ReactFlux.App.NavBar            as NavBar
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types



view :: Store -> HTMLView_ -> HTMLView_
view !store' !page' = do
  defineViewWithSKey "layout" (store', page') go
  where
  go :: (Store, HTMLView_) -> HTMLView_
  go (st@Store{..}, page) = do
    div_ ["key" $= "default-layout"] $ do
      Loader.loader1 _l_m_me $ \m_me -> do
        NavBar.view m_me _route
        Errors.view _errors
        Breadcrumbs.view _route
        page
