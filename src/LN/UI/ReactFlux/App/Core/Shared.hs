{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS  -fno-warn-orphans #-}

module LN.UI.ReactFlux.App.Core.Shared (
  Store (..),
  Action (..),
  store,
  defaultStore,
  dispatch
) where



import           Control.Concurrent         (forkIO)
import           Control.DeepSeq            (NFData)
import           Control.Monad              (void)
import           Data.Int                   (Int64)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           GHCJS.Router.Base          (setLocationHash)
import           React.Flux

import           LN.T.Pack.Sanitized.User   (UserSanitizedPackResponse (..))
import           LN.T.User                  (UserResponse (..))
import           LN.UI.Core.App
import           LN.UI.Core.Control
import           LN.UI.Core.Router          (Route (..), RouteWith,
                                             fromRouteWith, routeWith')
import           LN.UI.Core.State.Internal
import qualified LN.UI.ReactFlux.Dispatcher as Dispatcher



instance StoreData Store where

  type StoreAction Store = Action

  transform (Goto route_with) st = do
    putStrLn $ "Goto: " <> show route_with
    setLocationHash $ Text.unpack $ fromRouteWith route_with
    pure st

  transform action st@Store{..} = do
    putStrLn $ "Route: " <> show _route
    (result', st') <- runCore st Start action
    void $ forkIO $ basedOn result' action
    pure st'

    where
    basedOn result_ act_ = case result_ of
      Start              -> do
                            putStrLn "transform: Start"
                            pure ()
      Next               -> do
                            putStrLn "transform: Next"
                            Dispatcher.dispatch $ SomeStoreAction store (MachNext act_)
      Done               -> do
                            putStrLn "transform: Done"
                            pure ()
      Failure            -> do
                            putStrLn "transform: Failure"
                            pure ()
      Reroute route_with -> do
                            putStrLn "transform: Reroute"
                            Dispatcher.dispatch $ SomeStoreAction store (Goto route_with)



store :: ReactStore Store
store = mkStore defaultStore



dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]
