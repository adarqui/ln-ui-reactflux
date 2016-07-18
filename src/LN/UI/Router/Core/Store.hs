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



import           Control.DeepSeq        (NFData)
import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import           LN.T.Pack.User         (UserPackResponse (..))
import           LN.UI.Router.Class.App
import           React.Flux             hiding (view)
import qualified React.Flux             as RF



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
  = Core_SetHash Text
  | Core_Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData CoreStore where
  type StoreAction CoreStore = CoreAction
  transform action st = do
    putStrLn "Core"
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
defaultLayout st@CoreStore{..} page = do
  div_ $
    navBar coreStore_Me



navBar :: Maybe UserPackResponse -> ReactElementM ViewEventHandler ()
navBar m_user_pack =
  div_ $ p_ $ elemText "user"
