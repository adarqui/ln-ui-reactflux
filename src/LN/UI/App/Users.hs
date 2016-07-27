{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Users (
  Store,
  defaultStore,
  Action (..),
  store,
  view_,
  view
) where



import           Control.DeepSeq                 (NFData)
import           Control.Monad.Trans.Either      (EitherT, runEitherT)
import           Data.Int                        (Int64)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Rehtie                     (rehtie)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Haskell.Helpers.Either          (mustPassT)
import           React.Flux                      hiding (view)
import qualified React.Flux                      as RF
import qualified Web.Bootstrap3                  as B

import           LN.Api                          (getUserSanitizedPacks,
                                                  getUsersCount')
import           LN.T.Pack.Sanitized.User        (UserSanitizedPackResponse (..), UserSanitizedPackResponses (..))
import           LN.T.Size                       (Size (..))
import           LN.T.User                       (UserResponse (..),
                                                  UserSanitizedResponse (..))
import qualified LN.UI.App.Gravatar              as Gravatar
import           LN.UI.App.PageNumbers           (runPageInfo)
import qualified LN.UI.App.PageNumbers           as PageNumbers
import           LN.UI.App.Types                 (UsersMap)
import           LN.UI.Helpers.DataTime          (prettyUTCTimeMaybe)
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.Map               (idmapFrom)
import           LN.UI.Helpers.ReactFluxDOM      (ahref, ahrefName, className_,
                                                  classNames_)
import           LN.UI.Router                    (CRUD (..), Params, Route (..),
                                                  RouteWith (..), routeWith')
import           LN.UI.State.PageInfo            (PageInfo (..),
                                                  defaultPageInfo,
                                                  pageInfoFromParams,
                                                  paramsFromPageInfo)
import           LN.UI.Types                     (HTMLEvent_)



data Store = Store {
  _pageInfo :: PageInfo,
  _l_users  :: Map Int64 UserSanitizedPackResponse
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
        pure $ st{ _l_users = idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
                 , _pageInfo = new_page_info }



store :: ReactStore Store
store = mkStore defaultStore



defaultStore :: Store
defaultStore = Store {
  _pageInfo      = defaultPageInfo,
  _l_users = Map.empty
}



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineControllerView "users" store $ \Store{..} _ ->
  cldiv_ B.containerFluid $ do
    h1_ "Users"
    PageNumbers.view_ (_pageInfo, routeWith' $ Users Index)
    ul_ [className_ B.listUnstyled] $ do
      mapM_ (\UserSanitizedPackResponse{..} -> do
        let user = userSanitizedPackResponseUser
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs1 $ p_ $ Gravatar.viewUser_ XSmall user
            cldiv_ B.colXs3 $ p_ $ ahrefName (userSanitizedResponseDisplayName user) (routeWith' $ Users (ShowS $ userSanitizedResponseName user))
            cldiv_ B.colXs2 $ p_ $ elemText $ prettyUTCTimeMaybe $ userSanitizedResponseCreatedAt user
        ) _l_users
