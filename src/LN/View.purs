module LN.View (
  renderView
) where



import Data.Maybe                (Maybe(..))
import Halogen                   (HTML, ComponentHTML)
import Halogen.HTML.Indexed      as H
import Prelude                   (map, const, ($))

import LN.Input.Types            (Input)
import LN.Router.Types           (Routes(..), CRUD(..))
import LN.State.Types            (State)
import LN.View.Home              (renderView_Home)
import LN.View.About             (renderView_About)
import LN.View.Errors            (renderView_Errors)
import LN.View.Portal            (renderView_Portal)
import LN.View.Users.Index       (renderView_Users_Index)
import LN.View.Users.Show        (renderView_Users_Show)
import LN.View.Users.Profile     (renderView_Users_Profile)
import LN.View.Users.Settings    (renderView_Users_Settings)
import LN.View.Users.PMs         (renderView_Users_PMs)
import LN.View.Users.Threads     (renderView_Users_Threads)
import LN.View.Users.ThreadPosts (renderView_Users_ThreadPosts)
import LN.View.Users.Workouts    (renderView_Users_Workouts)
import LN.View.Users.Resources   (renderView_Users_Resources)
import LN.View.Users.Leurons     (renderView_Users_Leurons)
import LN.View.Users.Likes       (renderView_Users_Likes)
import LN.View.Four04            (renderView_404)

import LN.View.Organizations.Index                      (renderView_Organizations_Index)
import LN.View.Organizations.Show                       (renderView_Organizations_Show)
import LN.View.Organizations.Forums.Show                (renderView_Organizations_Forums_Show)
import LN.View.Organizations.Forums.Boards.Show         (renderView_Organizations_Forums_Boards_Show)
import LN.View.Organizations.Forums.Boards.Threads.Show (renderView_Organizations_Forums_Boards_Threads_Show)

import LN.View.Resources.Index                    (renderView_Resources_Index)
import LN.View.Resources.New                      (renderView_Resources_New)
-- TODO FIXME: import LN.View.Resources.Edit
import LN.View.Resources.Show                     (renderView_Resources_Show)
import LN.View.Resources.Leurons.Index            (renderView_Resources_Leurons_Index)
import LN.View.Resources.SiftLeurons              (renderView_Resources_SiftLeurons)
import LN.View.Resources.SiftLeuronsLinear.Index  (renderView_Resources_SiftLeuronsLinear_Index)
import LN.View.Resources.SiftLeuronsLinear.Show   (renderView_Resources_SiftLeuronsLinear_Show)
import LN.View.Resources.SiftLeuronsRandom        (renderView_Resources_SiftLeuronsRandom)

import LN.View.Leurons.Index               (renderView_Leurons_Index)
import LN.View.Leurons.Show                (renderView_Leurons_Show)



renderView :: Routes -> State -> ComponentHTML Input


renderView Home   = const $ renderView_Home


renderView About  = const $ renderView_About


renderView Errors = renderView_Errors


renderView Portal = const $ renderView_Portal


renderView (Users Index params)            = renderView_Users_Index
renderView (Users (Show user_name) params) = renderView_Users_Show user_name


renderView (UsersProfile user_name params)     = renderView_Users_Profile user_name
renderView (UsersSettings user_name params)    = renderView_Users_Settings user_name
renderView (UsersPMs user_name params)         = renderView_Users_PMs user_name
renderView (UsersThreads user_name params)     = renderView_Users_Threads user_name
renderView (UsersThreadPosts user_name params) = renderView_Users_ThreadPosts user_name
renderView (UsersWorkouts user_name params)    = renderView_Users_Workouts user_name
renderView (UsersResources user_name params)   = renderView_Users_Resources user_name
renderView (UsersLeurons user_name params)     = renderView_Users_Leurons user_name
renderView (UsersLikes user_name params)       = renderView_Users_Likes user_name


renderView (Organizations Index params)           = renderView_Organizations_Index
renderView (Organizations (Show org_name) params) = renderView_Organizations_Show org_name


renderView (OrganizationsForums org_name (Show forum_name) params) = renderView_Organizations_Forums_Show forum_name


renderView (OrganizationsForumsBoards org_name forum_name (Show board_name) params) = renderView_Organizations_Forums_Boards_Show board_name


renderView (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread_name) params) = renderView_Organizations_Forums_Boards_Threads_Show thread_name



renderView (Resources Index params)              = renderView_Resources_Index
renderView (Resources New params)                = renderView_Resources_New
renderView (Resources (ShowI resource_id) params) = renderView_Resources_Show resource_id



renderView (ResourcesLeurons resource_id Index params)                    = renderView_Resources_Leurons_Index resource_id
renderView (ResourcesSiftLeurons resource_id params)                      = renderView_Resources_SiftLeurons resource_id
renderView (ResourcesSiftLeuronsLinear resource_id Index params)          = renderView_Resources_SiftLeuronsLinear_Index resource_id
renderView (ResourcesSiftLeuronsLinear resource_id (ShowI offset) params) = renderView_Resources_SiftLeuronsLinear_Show resource_id offset
renderView (ResourcesSiftLeuronsRandom resource_id params)                = renderView_Resources_SiftLeuronsRandom resource_id


renderView (Leurons Index params)              = renderView_Leurons_Index
renderView (Leurons (ShowI resource_id) params) = renderView_Leurons_Show resource_id



renderView _ = const $ renderView_404



errs :: forall a b. Maybe (Array String) -> HTML a b
errs Nothing = H.div_ []
errs (Just errors) = H.div_ (map (\e -> H.p_ [H.text e]) errors)
