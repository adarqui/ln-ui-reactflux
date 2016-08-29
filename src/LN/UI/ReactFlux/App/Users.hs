{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Users (
  viewIndex,
  viewIndex_,
  viewShowS
) where



import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Int                              (Int64)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Rehtie                           (rehtie)
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Haskell.Helpers.Either                (mustPassT)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.Api                                (getUserSanitizedPacks,
                                                        getUsersCount')
import           LN.T.Pack.Sanitized.User              (UserSanitizedPackResponse (..), UserSanitizedPackResponses (..))
import           LN.T.Size                             (Size (..))
import           LN.T.User                             (UserResponse (..), UserSanitizedResponse (..))
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.HaskellApiHelpers  (rd)
import           LN.UI.Core.Helpers.Map                (idmapFrom)
import           LN.UI.Core.PageInfo                   (PageInfo (..),
                                                        defaultPageInfo,
                                                        pageInfoFromParams,
                                                        paramsFromPageInfo)
import           LN.UI.Core.Router                     (CRUD (..), Params,
                                                        Route (..),
                                                        RouteWith (..),
                                                        routeWith')
import           LN.UI.Core.Types
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import           LN.UI.ReactFlux.App.Types             (UsersMap)
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefName,
                                                        className_, classNames_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types



viewIndex :: PageInfo -> Loader (Map UserId UserSanitizedPackResponse) -> HTMLView_
viewIndex !page_info' !l_users' = do
  defineViewWithSKey "users-index-1" (page_info', l_users') $ \(page_info, l_users) -> do
    cldiv_ B.containerFluid $ do
      h1_ "Users"
      Loader.loader1 l_users (viewIndex_ page_info)



viewIndex_ :: PageInfo -> Map UserId UserSanitizedPackResponse -> HTMLView_
viewIndex_ !page_info' !users' = do
  defineViewWithSKey "users-index-2" (page_info', users') $ \(page_info, users) -> do
    PageNumbers.view page_info (routeWith' $ Users Index)
    ul_ [className_ B.listUnstyled] $ do
      forM_ users $ \UserSanitizedPackResponse{..} -> do
        let user = userSanitizedPackResponseUser
        li_ $ do
          cldiv_ B.row $ do
            cldiv_ B.colXs1 $ p_ $ Gravatar.viewUser XSmall user
            cldiv_ B.colXs3 $ p_ $ ahrefName (userSanitizedResponseDisplayName user) (routeWith' $ Users (ShowS $ userSanitizedResponseName user))
            cldiv_ B.colXs2 $ p_ $ elemText $ prettyUTCTimeMaybe $ userSanitizedResponseCreatedAt user




viewShowS
  :: PageInfo
  -> UserId
  -> Loader (Maybe UserSanitizedPackResponse)
  -> HTMLView_

viewShowS !page_info' !me_id' !l_m_user' = do
  defineViewWithSKey "users-show-1" (page_info', me_id', l_m_user') $ \(page_info, me_id, l_m_user) -> do
    Loader.maybeLoader1 l_m_user (go page_info me_id)
  where
  go page_info me_id user = do
    let
      UserSanitizedPackResponse{..} = user
      UserSanitizedResponse{..}     = userSanitizedPackResponseUser
    cldiv_ B.pageHeader $ do
      -- TODO FIXME: h1_ [className_ B.textCenter] $ elemText userSanitizedResponseDisplayName
      ahref $ routeWith' (UsersProfile userSanitizedResponseName Index)
