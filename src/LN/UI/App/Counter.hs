{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- |

module LN.UI.App.Counter where

import           React.Flux         hiding (view)
import qualified React.Flux         as RF

import           Control.Concurrent (forkIO, threadDelay)
import           Control.DeepSeq
import           Control.Monad      (void)
import           Data.Typeable      (Typeable)
import           GHC.Generics       (Generic)


data CounterState = CounterState {csCounter :: !(Maybe Int)
                             }
                deriving (Show, Typeable)

data CounterAction = CounterInit
                 | CounterInc
                 | CounterDec
                 deriving (Show, Typeable, Generic, NFData)

instance StoreData CounterState where
  type StoreAction CounterState = CounterAction
  transform action st@CounterState{..} = do
    putStrLn $ "Action: " ++ show action
    case action of
      CounterInit -> do
--        void $ forkIO $ workerLoop store
        pure st
      CounterInc ->
        pure $ st{csCounter = fmap succ csCounter}
      CounterDec ->
        pure $ st{csCounter = fmap pred csCounter}
    where
--      workerLoop rst = do
--        storeWorker rst
--        workerLoop rst
--      storeWorker rst = do
--        threadDelay 1000000
--        alterStore rst CounterInc

store :: ReactStore CounterState
store = mkStore $ CounterState Nothing

view :: ReactView CounterState
view = defineView "counter" $ \st ->
  div_ $  maybe mempty elemShow $ csCounter st

view_ :: CounterState -> ReactElementM eventHandler ()
view_ st =
  RF.view view st mempty
