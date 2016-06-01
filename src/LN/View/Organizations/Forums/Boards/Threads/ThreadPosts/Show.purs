module LN.View.Organizations.Forums.Boards.Threads.ThreadPosts.Show (
  renderView_Organizations_Forums_Boards_Threads_ThreadPosts_Show
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>), (-))

import LN.Input.Types                  (Input (..))
import LN.Input.ThreadPost             (InputThreadPost (..))
import LN.Router.Link                  (linkTo, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup, usersMapLookup_ToNick, usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.Like             (renderLike)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Ent(..)
                                       , UserSanitizedPackResponse, ThreadPackResponse, BoardPackResponse
                                       , ForumPackResponse, OrganizationPackResponse, PostData(PostDataBBCode)
                                       , Size(Medium), ThreadPostStatResponse(ThreadPostStatResponse)
                                       , _UserSanitizedStatResponse, stat_, _UserSanitizedPackResponse
                                       , signature_, _ProfileResponse, profile_, _ThreadPostStatResponse
                                       , _ThreadPostPackResponse, _ThreadPostResponse, threadPost_, body_
                                       , _ThreadPostRequest, _ThreadResponse, thread_, _ThreadPackResponse
                                       , _BoardResponse, board_, _BoardPackResponse, _ForumResponse, forum_
                                       , _ForumPackResponse, _OrganizationResponse, organization_
                                       , _OrganizationPackResponse)



renderView_Organizations_Forums_Boards_Threads_ThreadPosts_Show :: Int -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Threads_ThreadPosts_Show thread_post_id st = H.div_ [H.text "posts show"]
