module LN.UI.Main (
  runMain
) where



import           Control.Monad.IO.Class    (liftIO)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Router



runMain :: IO ()
runMain = mainWidget $ do
  el "div" $ do
    el "h1" $ text "reflex-starter"
    el "a" $ text "Now you can start playing with Reflex!"
