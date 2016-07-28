{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}

module LN.UI.ReactFlux.Types (
  module A,
  HTMLView,
  HTMLView_,
  HTMLEvent,
  HTMLEvent_
) where



import           Data.Int         (Int64)
import           Data.Text        (Text)
import           Prelude
import           React.Flux

import           LN.UI.Core.Types as A



-- type HTML = ReactElementM EventHandler
-- type HTML' = HTML ()

type HTMLView  = ReactElementM ViewEventHandler
type HTMLView_ = ReactElementM ViewEventHandler ()



type HTMLEvent = forall eventHandler. ReactElementM eventHandler
type HTMLEvent_ = forall eventHandler. ReactElementM eventHandler ()
