module LN.UI.ReactFlux.Dispatch (
    dispatcher
  , dispatch
) where



import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           React.Flux
import Data.IORef
import System.IO.Unsafe



type Dispatch  = SomeStoreAction
type DispatchV = MVar Dispatch



ref :: IORef DispatchV
{-# NOINLINE ref #-}
ref = unsafePerformIO (newEmptyMVar >>= newIORef)



dispatcher :: IO ()
dispatcher = do
  mv <- readIORef ref
  void $ forkIO $ runDispatcher mv



runDispatcher :: DispatchV -> IO ()
runDispatcher mv = do
  forever $ do
    action <- takeMVar mv
    print "dispatched"
    executeAction action



dispatch :: Dispatch -> IO ()
dispatch action = do
  mv <- readIORef ref
  putMVar mv action
  print "dispatch"
