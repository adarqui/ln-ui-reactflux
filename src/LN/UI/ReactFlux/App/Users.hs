{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Users (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view,
  viewIndex,
  viewIndex_,
  viewIndex__
) where



import           Control.DeepSeq                      (NFData)
import           Control.Monad.Trans.Either           (EitherT, runEitherT)
import           Data.Int                             (Int64)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Rehtie                          (rehtie)
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           Haskell.Helpers.Either               (mustPassT)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import qualified Web.Bootstrap3                       as B

import           LN.Api                               (getUserSanitizedPacks,
                                                       getUsersCount')
import           LN.T.Pack.Sanitized.User             (UserSanitizedPackResponse (..), UserSanitizedPackResponses (..))
import           LN.T.Size                            (Size (..))
import           LN.T.User                            (UserResponse (..), UserSanitizedResponse (..))
import           LN.UI.Core.Helpers.DataTime          (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Core.Helpers.Map               (idmapFrom)
import           LN.UI.Core.PageInfo                  (PageInfo (..),
                                                       defaultPageInfo,
                                                       pageInfoFromParams,
                                                       paramsFromPageInfo)
import           LN.UI.Core.Router                    (CRUD (..), Params,
                                                       Route (..),
                                                       RouteWith (..),
                                                       routeWith')
import           LN.UI.Core.Types
import qualified LN.UI.ReactFlux.App.Gravatar         as Gravatar
import           LN.UI.ReactFlux.App.Loader           (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader           as Loader
import           LN.UI.ReactFlux.App.PageNumbers      (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers      as PageNumbers
import           LN.UI.ReactFlux.App.Types            (UsersMap)
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahref, ahrefName,
                                                       className_, classNames_)
import           LN.UI.ReactFlux.Types



data Store = Store {
  _pageInfo :: PageInfo,
  _l_users  :: Loader (Map Int64 UserSanitizedPackResponse)
}



data Action
  = Init Params
  | Nop
  deriving (Show, Typeable, Generic, NFData)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st = do
    putStrLn "Users"

    case action of
      Nop              -> pure st
      Init params  -> actions_init params

    where
    actions_init params = do
      let
        page_info   = pageInfoFromParams params
        params_list = paramsFromPageInfo page_info
      lr <- runEitherT $ do
        count         <- mustPassT $ rd $ getUsersCount'
        users <- mustPassT $ rd $ getUserSanitizedPacks params_list
        pure (count, users)
      rehtie lr (const $ pure st) $ \(count, user_packs) -> do
        let new_page_info = runPageInfo count page_info
        pure $ st{ _l_users = Loaded $ idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
                 , _pageInfo = new_page_info }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _pageInfo      = defaultPageInfo,
  _l_users = Loaded Map.empty
}



view_ :: CRUD -> HTMLEvent_
view_ crud =
  RF.view view crud mempty



view :: ReactView CRUD
view = defineControllerView "users" store $ \st@Store{..} crud ->
  mempty



viewIndex :: Store -> HTMLView_
viewIndex Store{..} = viewIndex_ _pageInfo _l_users



viewIndex_ :: PageInfo -> Loader (Map UserId UserSanitizedPackResponse) -> HTMLView_
viewIndex_ page_info l_users = do
  Loader.loader1 l_users (viewIndex__ page_info)



viewIndex__ :: PageInfo -> Map UserId UserSanitizedPackResponse -> HTMLView_
viewIndex__ page_info users = do
  cldiv_ B.containerFluid $ do
    h1_ "Users"
    PageNumbers.view_ (page_info, routeWith' $ Users Index)
    ul_ [className_ B.listUnstyled] $ do
      mapM_ (\UserSanitizedPackResponse{..} -> do
        let user = userSanitizedPackResponseUser
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs1 $ p_ $ Gravatar.viewUser_ XSmall user
            cldiv_ B.colXs3 $ p_ $ ahrefName (userSanitizedResponseDisplayName user) (routeWith' $ Users (ShowS $ userSanitizedResponseName user))
            cldiv_ B.colXs2 $ p_ $ elemText $ prettyUTCTimeMaybe $ userSanitizedResponseCreatedAt user
        ) users
