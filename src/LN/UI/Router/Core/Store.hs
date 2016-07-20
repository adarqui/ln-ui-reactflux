{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.Router.Core.Store (
  CoreStore (..),
  defaultCoreStore,
  CoreAction (..),
  coreStore,
  coreView,
  coreView_
) where



import           Control.DeepSeq            (NFData)
import           Control.Monad              (void)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           LN.Api                     (getMe')
import           LN.T.Pack.User             (UserPackResponse (..))
import           LN.UI.HaskellApiHelpers    (rd)
import           LN.UI.ReactFlux.DOM        (ahref)
import           LN.UI.Router.Class.App
import           LN.UI.Router.Class.Routes2
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF



data CoreStore = CoreStore {
  coreStore_Route :: RoutingApp,
  coreStore_Me    :: Maybe UserPackResponse
} deriving (Show, Typeable, Generic, NFData)

defaultCoreStore :: CoreStore
defaultCoreStore = CoreStore {
  coreStore_Route = AppHome,
  coreStore_Me    = Nothing
}



data CoreAction
  = Core_Init
  | Core_SetHash Text
  | Core_Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData CoreStore where
  type StoreAction CoreStore = CoreAction
  transform action st@CoreStore{..} = do
    case action of
      Core_Init -> action_core_init
      _         -> pure st
    where
    action_core_init = do
      putStrLn "Core_Init"
      void $ rd getMe'
      pure st



coreStore :: ReactStore CoreStore
coreStore = mkStore defaultCoreStore



coreView :: ReactView CoreStore
coreView =
  defineView "core" $ \st ->
    defaultLayout st (div_ $ p_ $ elemText "page")



coreView_ :: CoreStore -> ReactElementM eventHandler ()
coreView_ st =
  RF.view coreView st mempty



defaultLayout :: CoreStore -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
defaultLayout st@CoreStore{..} page =
  div_ $
    navBar coreStore_Me



navBar :: Maybe UserPackResponse -> ReactElementM ViewEventHandler ()
navBar m_user_pack =
  div_ $ do
    ahref $ routeWith' Home
    ahref $ routeWith' About
    ahref $ routeWith' Portal
    case m_user_pack of
      Nothing                   -> ahref $ routeWith' Login
      Just UserPackResponse{..} -> ahref $ routeWith' Logout -- : user
