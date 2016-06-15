module LN.View.ThreadPosts.Shared (
  displayUserStats,
  displayPostStats,
  displayPostData,
  postDataToBody
) where



import Data.Either                     (Either(..))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>), (-), (<<<))

import Daimyo.Data.ArrayList           (listToArray)

import Data.BBCode.HTML
import Data.BBCode.Parser
import Data.BBCode.Types

import LN.Input.Types                  (Input(..), cThreadPostMod)
import LN.Input.ThreadPost             (InputThreadPost(..), ThreadPost_Mod(..))
import LN.Router.Link                  (linkTo, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.ThreadPost             (ThreadPostRequestState)
import LN.State.PageInfo               (PageInfo)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup', usersMapLookup_ToNick', usersMapLookup_ToUser')
import LN.View.Helpers                 (showBadge')
import LN.T                            ( Ent(..)
                                       , UserSanitizedPackResponse, ThreadPackResponse, BoardPackResponse
                                       , ForumPackResponse, OrganizationPackResponse, PostData(..)
                                       , ThreadPostPackResponse, ThreadPostRequest
                                       , Size(Medium), ThreadPostStatResponse(ThreadPostStatResponse)
                                       , _UserSanitizedStatResponse, stat_, _UserSanitizedPackResponse
                                       , signature_, _ProfileResponse, profile_, _ThreadPostStatResponse
                                       , _ThreadPostPackResponse, _ThreadPostResponse, threadPost_, body_
                                       , _ThreadPostRequest, _ThreadResponse, thread_, _ThreadPackResponse
                                       , _BoardResponse, board_, _BoardPackResponse, _ForumResponse, forum_
                                       , _ForumPackResponse, _OrganizationResponse, organization_
                                       , _OrganizationPackResponse
                                       , like_, star_)



displayUserStats :: Maybe UserSanitizedPackResponse -> HTML _ _
displayUserStats Nothing = H.p_ [H.text "No stats."]
displayUserStats (Just user) =
  H.div_ [
    showBadge' "respect "   stats.respect,
    showBadge' "threads "   stats.threads,
    showBadge' "posts "     stats.threadPosts,
    showBadge' "workouts "  stats.workouts,
    showBadge' "resources " stats.resources,
    showBadge' "leurons "   stats.leurons
  ]
  where
  stats = user ^. _UserSanitizedPackResponse .. stat_ ^. _UserSanitizedStatResponse



displayPostStats :: ThreadPostStatResponse -> HTML _ _
displayPostStats (ThreadPostStatResponse stats) =
  H.div_ [
    showBadge' "score: "   $ stats.likes - stats.dislikes,
    showBadge' "up: "      stats.likes,
    showBadge' "neutral: " stats.neutral,
    showBadge' "down: "    stats.dislikes,
    showBadge' "stars: "   stats.stars,
    showBadge' "views: "   stats.views
  ]



displayPostData :: PostData -> ComponentHTML Input
displayPostData body =
  case body of
    PostDataEmpty      -> H.p_ []
    PostDataRaw v      -> H.p_ [H.text v]
    PostDataBBCode v   ->
      case parseBBCode v of
           Left err    -> H.p_ [H.text "error: ", H.text err]
           Right codes -> H.p_ $ runBBCodeToHTML codes
    PostDataMarkdown v -> H.p_ [H.text "markdown"]
    _                  -> H.p_ [H.text "unknown post body"]



postDataToBody :: PostData -> String
postDataToBody (PostDataBBCode v) = v
postDataToBody _                  = ""
