{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.ReactFlux.App.Gravatar (
  view_,
  view,
  viewUser_,
  viewOrganization_,
  gravatarSize,
  gravatarSizeParam
) where



import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (unpack)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.T.Organization          (OrganizationResponse (..))
import           LN.T.Size                  (Size (..))
import           LN.T.User                  (UserSanitizedResponse (..))
import           LN.UI.Core.Helpers.DataText     (tshow)
import           LN.UI.Core.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (ahrefElement)
import           LN.UI.Core.Router               (CRUD (..), Route (..),
                                             RouteWith (..), routeWith')
import           LN.UI.ReactFlux.Types                (HTMLEvent_)



-- | Renders a gravatar based on size, email md5, and alternate text
--
view_ :: RouteWith -> Size -> Text -> Text -> HTMLEvent_
view_ route_with size emailMD5 alt =
  RF.view view (route_with, size, emailMD5, alt) mempty



view :: ReactView (RouteWith, Size, Text, Text)
view = defineView "gravatar" $ \(route_with, size, emailMD5, alt) -> do
  let alt' = textToJSString' alt
  ahrefElement route_with $ img_ ["src" $= (gravatarUrlFrom'JSS size emailMD5), "alt" $= alt'] mempty



viewUser_ :: Size -> UserSanitizedResponse -> HTMLEvent_
viewUser_ size UserSanitizedResponse{..} =
  RF.view view (routeWith' (Users (ShowS userSanitizedResponseName)), size, userSanitizedResponseEmailMD5, userSanitizedResponseName) mempty



viewOrganization_ :: Size -> OrganizationResponse -> HTMLEvent_
viewOrganization_ size OrganizationResponse{..} =
  RF.view view (routeWith' (Organizations (ShowS organizationResponseName)), size, organizationResponseEmailMD5, organizationResponseName) mempty



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
