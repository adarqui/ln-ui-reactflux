{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.App.Loading (
  Loader (..),
  loader1,
  loader2,
  loader3,
  loadingImg
) where



import           React.Flux  hiding (view)

import           LN.UI.Types (HTMLView_)



data Loader a
  = Loaded a
  | Loading
  | CantLoad



loader1 :: forall v. Loader v -> (v -> HTMLView_) -> HTMLView_
loader1 loading_v loaded =
  case loading_v of
    Loaded v -> loaded v
    Loading  -> loadingImg
    CantLoad -> unexpectedError



loader2 :: forall v1 v2. Loader v1 -> Loader v2 -> (v1 -> v2 -> HTMLView_) -> HTMLView_
loader2 loading_v1 loading_v2 loaded =
  case (loading_v1, loading_v2) of
    (Loaded v1, Loaded v2) -> loaded v1 v2
    (Loading, _)           -> loadingImg
    (_, Loading)           -> loadingImg
    _                      -> unexpectedError



loader3 :: forall v1 v2 v3. Loader v1 -> Loader v2 -> Loader v3 -> (v1 -> v2 -> v3 -> HTMLView_) -> HTMLView_
loader3 loading_v1 loading_v2 loading_v3 loaded =
  case (loading_v1, loading_v2, loading_v3) of
    (Loaded v1, Loaded v2, Loaded v3) -> loaded v1 v2 v3
    (Loading, _, _)                   -> loadingImg
    (_, Loading, _)                   -> loadingImg
    (_, _, Loading)                   -> loadingImg
    _                                 -> unexpectedError



loadingImg :: HTMLView_
loadingImg = img_ ["src" $= "/static/img/loading/2.gif", "alt" $= "loading"] mempty


unexpectedError :: HTMLView_
unexpectedError = div_ $ p_ $ elemText "Unexpected error loading resource."
