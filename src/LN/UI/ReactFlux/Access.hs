{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

module LN.UI.ReactFlux.Access (
  module A
) where



import           React.Flux

import           LN.T
import           LN.UI.Core.Access     as A
import           LN.UI.ReactFlux.Types



instance HasAccess (ReactElementM ViewEventHandler ()) where
  open = div_
  empty = mempty
