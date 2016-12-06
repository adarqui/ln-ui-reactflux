{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.ReactFlux.App.Gravatar (
  view,
  viewUser,
  viewOrganization,
  gravatarSize,
  gravatarSizeParam
) where



import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                             as Text (unpack)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF

import           LN.T.Organization                     (OrganizationResponse (..))
import           LN.T.Size                             (Size (..))
import           LN.T.User                             (UserSanitizedResponse (..))
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.GHCJS              (JSString,
                                                        textToJSString')
import           LN.UI.Core.Router                     (CRUD (..), Route (..),
                                                        RouteWith (..),
                                                        routeWith')
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types                 (HTMLView_)



-- | Renders a gravatar based on size, email md5, and alternate text
--
view :: RouteWith -> Size -> Text -> Text -> HTMLView_
view !route_with' !size' !emailMD5' !alt' =
  defineViewWithSKey "gravatar" (route_with', size', emailMD5', alt') go
  where
  go :: (RouteWith, Size, Text, Text) -> HTMLView_
  go (route_with, size, emailMD5, alt) = do
    let alt' = textToJSString' alt
    ahrefElement route_with $ img_ ["key" $= ("gravatar-img-" <> textToJSString' emailMD5), "src" $= (gravatarUrlFrom'JSS size emailMD5), "alt" $= alt'] mempty



viewUser :: Size -> UserSanitizedResponse -> HTMLView_
viewUser size UserSanitizedResponse{..} =
  view (routeWith' $ Users (ShowS userSanitizedResponseName)) size userSanitizedResponseEmailMD5 userSanitizedResponseName



viewOrganization :: Size -> OrganizationResponse -> HTMLView_
viewOrganization size OrganizationResponse{..} =
  view (routeWith' $ Organizations (ShowS organizationResponseName)) size organizationResponseEmailMD5 organizationResponseName



gravatarUrlFrom :: Size -> Text -> Text
gravatarUrlFrom sz emailMD5 = "//www.gravatar.com/avatar/" <> emailMD5 <> "?d=identicon&r=pg" <> "&" <> (gravatarSizeParam sz)



gravatarUrlFrom'JSS :: Size -> Text -> JSString
gravatarUrlFrom'JSS = (textToJSString' .) . gravatarUrlFrom



gravatarSize :: Size -> Int
gravatarSize sz =
  case sz of
    XSmall -> 20
    Small  -> 40
    Medium -> 60
    Large  -> 80
    XLarge -> 100



gravatarSizeParam :: Size -> Text
gravatarSizeParam sz = "s=" <> tshow (gravatarSize sz)
