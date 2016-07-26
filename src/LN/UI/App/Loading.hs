{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

module LN.UI.App.Loading (
  Loader (..),
  loader1
) where



import React.Flux hiding (view)



data Loader a
  = Loaded a
  | Loading
  | CantLoad



loader1 :: forall v. Loader v -> (v -> HTMLView_) -> HTMLView_
loader1 loading_v loaded =
  case loading_v of
    Loaded v -> loaded v
    Loading  -> img_ ["src" $= "/static/img/loading/2.gif", "alt" $= "loading"] mempty
    CantLoad -> div_ $ p_ $ elemText "Unexpected error loading resource."
