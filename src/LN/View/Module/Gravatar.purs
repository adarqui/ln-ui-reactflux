module LN.View.Module.Gravatar (
  renderGravatarForUser,
  gravatarSize,
  gravatarSizeParam,
  gravatarUrlFromUser,
  gravatarUrlFromOrganization
) where



import Data.Maybe                         (Maybe(..))
import Halogen                            (ComponentHTML)
import Halogen.HTML.Indexed               as H
import Halogen.HTML.Properties.Indexed    as P
import Prelude                            (show, ($), (<>))

import LN.Input.Types                     (Input)
import LN.Router.Link                     (linkTo', linkToHref)
import LN.Router.Types                    (Routes(..), CRUD(..))
import LN.T



{-
renderGravatar :: String -> ComponentHTML Input
renderGravatar md5 =
  H.img [P.src $ "//www.gravatar.com/avatar/" <> md5 <> "?r=pg", linkToHref (Users (Show]
  -}


renderGravatarForUser :: Size -> Maybe UserSanitizedResponse -> ComponentHTML Input
renderGravatarForUser sz Nothing =
  H.img [P.src $ "//www.gravatar.com/avatar/none?d=identicon&" <> (gravatarSizeParam sz)]
renderGravatarForUser sz (Just (UserSanitizedResponse user)) =
  linkTo'
    (Users (Show user.nick) [])
    [H.img [P.src $ "//www.gravatar.com/avatar/" <> user.emailMD5 <> "?d=identicon&r=pg" <> "&" <> (gravatarSizeParam sz), P.alt user.nick]]


gravatarSize :: Size -> Int
gravatarSize sz =
  case sz of
    XSmall -> 20
    Small  -> 40
    Medium -> 60
    Large  -> 80
    XLarge -> 100



gravatarSizeParam :: Size -> String
gravatarSizeParam sz = "s=" <> show (gravatarSize sz)


-- TODO CLEANUP


gravatarUrlFromUser :: Size -> UserSanitizedResponse -> String
gravatarUrlFromUser sz (UserSanitizedResponse user) =
  "//www.gravatar.com/avatar/" <> user.emailMD5 <> "?d=identicon&r=pg" <> "&" <> (gravatarSizeParam sz)



gravatarUrlFromOrganization :: Size -> OrganizationResponse -> String
gravatarUrlFromOrganization sz (OrganizationResponse org) =
  "//www.gravatar.com/avatar/" <> org.emailMD5 <> "?d=identicon&r=pg" <> "&" <> (gravatarSizeParam sz)



{-
gravatarFromUserId :: State -> Int -> ComponentHTML Input
gravatarFromUserId st user_id =
  H.div_ [
    linkTo' (Users $ Show user.nick) [H.img [P.src $ "//www.gravatar.com/avatar/" <> user.emailMD5 <> "?r=pg", P.alt user.nick]]
  ]
  where
    -}
