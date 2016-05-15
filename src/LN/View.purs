module LN.View (
  renderView
) where



import Data.Maybe (Maybe(..))
import Halogen
import Halogen.HTML.Indexed as H
import Prelude (map, const, ($))
import LN.Input.Types (Input)
import LN.Router.Types (Routes(..), CRUD(..))
import LN.State.Types (State)
import LN.View.Home
import LN.View.About
import LN.View.Portal
import LN.View.Portal.Organizations
import LN.View.Portal.Users
import LN.View.Portal.Resources
import LN.View.Portal.Leurons
import LN.View.Users.Show
import LN.View.Users.Profile
import LN.View.Users.Settings
import LN.View.Users.PMs
import LN.View.Users.Threads
import LN.View.Users.ThreadPosts
import LN.View.Users.Workouts
import LN.View.Users.Resources
import LN.View.Users.Leurons
import LN.View.Users.Likes
import LN.View.Organizations.Show
import LN.View.Organizations.Forums.Show
import LN.View.Organizations.Forums.Boards.Show
import LN.View.Organizations.Forums.Boards.Threads.Show
import LN.View.Resources.Index
import LN.View.Resources.New
import LN.View.Resources.Edit
import LN.View.Resources.Show
import LN.View.Four04



renderView :: Routes -> State -> ComponentHTML Input


renderView Home = const $ renderView_Home


renderView About = const $ renderView_About


renderView Portal                   = const $ renderView_Portal
renderView PortalOrganizations      = renderView_Portal_Organizations
renderView (PortalUsers params)     = renderView_Portal_Users
renderView (PortalResources params) = renderView_Portal_Resources
renderView (PortalLeurons params)   = renderView_Portal_Leurons


renderView (Users (Show user_name)) = renderView_Users_Show user_name


renderView (UsersProfile user_name params) = renderView_Users_Profile user_name
renderView (UsersSettings user_name params) = renderView_Users_Settings user_name
renderView (UsersPMs user_name params) = renderView_Users_PMs user_name
renderView (UsersThreads user_name params) = renderView_Users_Threads user_name
renderView (UsersThreadPosts user_name params) = renderView_Users_ThreadPosts user_name
renderView (UsersWorkouts user_name params) = renderView_Users_Workouts user_name
renderView (UsersResources user_name params) = renderView_Users_Resources user_name
renderView (UsersLeurons user_name params) = renderView_Users_Leurons user_name
renderView (UsersLikes user_name params) = renderView_Users_Likes user_name


renderView (Organizations (Show org_name)) = renderView_Organizations_Show org_name


renderView (OrganizationsForums org_name (Show forum_name) params) = renderView_Organizations_Forums_Show forum_name


renderView (OrganizationsForumsBoards org_name forum_name (Show board_name) params) = renderView_Organizations_Forums_Boards_Show board_name


renderView (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread_name) params) = renderView_Organizations_Forums_Boards_Threads_Show thread_name

renderView (Resources Index params) = renderView_Resources_Index
renderView (Resources New params) = renderView_Resources_New
renderView (Resources (Show resource_id) params) = renderView_Resources_Show resource_id

renderView _ = const $ renderView_404



errs :: forall a b. Maybe (Array String) -> HTML a b
errs Nothing = H.div_ []
errs (Just errors) = H.div_ (map (\e -> H.p_ [H.text e]) errors)
