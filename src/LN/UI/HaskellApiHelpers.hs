-- | Haskell API Helpers helpers
--

{-# LANGUAGE OverloadedStrings #-}

module LN.UI.HaskellApiHelpers (
  rd
) where



import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Default               (def)
import           Haskell.Api.Helpers        (SpecificApiOptions)
import           Haskell.Api.Helpers.Shared (ApiOptions (..), runWith)



defaultApiOptionsUI :: ApiOptions SpecificApiOptions
defaultApiOptionsUI = ApiOptions {
  apiUrl         = "",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Nothing,
  apiOptions     = def,
  apiDebug       = True
}



rd :: ReaderT (ApiOptions SpecificApiOptions) m a -> m a
rd = flip runWith defaultApiOptionsUI
