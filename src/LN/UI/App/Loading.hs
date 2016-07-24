{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

module LN.UI.App.Loading (
  Loader (..),
  loader1,
  view_,
  view
) where



import React.Flux hiding (view)
import qualified React.Flux as RF



data Loader a
  = Loaded a
  | Loading
  | CantLoad



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty



view :: ReactView ()
view =
  defineView "core" $ \_ ->
    img_ ["src" $= "/static/img/loading/2.gif", "alt" $= "loading"] mempty



loader1 :: forall v. Loader v -> (v -> ReactElementM ViewEventHandler ()) -> ReactElementM ViewEventHandler ()
loader1 loading_v loaded =
  case loading_v of
    Loaded v -> loaded v
    Loading  -> img_ ["src" $= "/static/img/loading/2.gif", "alt" $= "loading"] mempty
    CantLoad -> div_ $ p_ $ elemText "Unexpected error loading resource."
