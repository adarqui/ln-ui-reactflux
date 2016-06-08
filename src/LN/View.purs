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
import LN.View.Organizations.Mod                        ( renderView_Organizations_New, renderView_Organizations_Edit
                                                        , renderView_Organizations_Delete)
import LN.View.Organizations.Show                       (renderView_Organizations_Show)
import LN.View.Organizations.Forums.Show                (renderView_Organizations_Forums_Show)
import LN.View.Organizations.Forums.Boards.Show         (renderView_Organizations_Forums_Boards_Show)
import LN.View.Organizations.Forums.Boards.Threads.Show (renderView_Organizations_Forums_Boards_Threads_Show)
import LN.View.Organizations.Forums.Boards.Threads.ThreadPosts.Show (renderView_Organizations_Forums_Boards_Threads_ThreadPosts_Show)

import LN.View.Forums.Mod                        ( renderView_Forums_New, renderView_Forums_Edit
                                                 , renderView_Forums_Delete
                                                 , renderView_Forums_NewS, renderView_Forums_EditS
                                                 , renderView_Forums_DeleteS)

import LN.View.Boards.Mod                        ( renderView_Boards_New, renderView_Boards_Edit
                                                 , renderView_Boards_Delete
                                                 , renderView_Boards_NewS, renderView_Boards_EditS
                                                 , renderView_Boards_DeleteS)

import LN.View.Threads.Mod                       ( renderView_Threads_New, renderView_Threads_Edit
                                                 , renderView_Threads_Delete
                                                 , renderView_Threads_NewS, renderView_Threads_EditS
                                                 , renderView_Threads_DeleteS)

import LN.View.ThreadPosts.Mod                   ( renderView_ThreadPosts_New, renderView_ThreadPosts_Edit
                                                 , renderView_ThreadPosts_Delete)

import LN.View.ThreadPosts.Show                  (renderView_ThreadPosts_Show)

import LN.View.Resources.Index                   (renderView_Resources_Index)
import LN.View.Resources.Mod                     ( renderView_Resources_New, renderView_Resources_Edit
                                                 , renderView_Resources_Delete)
import LN.View.Resources.Show                    (renderView_Resources_Show)
import LN.View.Resources.Leurons.Index           (renderView_Resources_Leurons_Index)
import LN.View.Resources.SiftLeurons             (renderView_Resources_SiftLeurons)
import LN.View.Resources.SiftLeuronsLinear.Index (renderView_Resources_SiftLeuronsLinear_Index)
import LN.View.Resources.SiftLeuronsLinear.Show  (renderView_Resources_SiftLeuronsLinear_Show)
import LN.View.Resources.SiftLeuronsRandom       (renderView_Resources_SiftLeuronsRandom)

import LN.View.Leurons.Index               (renderView_Leurons_Index)
import LN.View.Leurons.Show                (renderView_Leurons_Show)
import LN.View.Leurons.Mod                 (renderView_Leurons_New, renderView_Leurons_Edit, renderView_Leurons_Delete)



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
renderView (Organizations New params)             = renderView_Organizations_New
renderView (Organizations (EditI org_id) params)  = renderView_Organizations_Edit org_id
renderView (Organizations (DeleteI org_id) params)= renderView_Organizations_Delete org_id
renderView (Organizations (Show org_name) params) = renderView_Organizations_Show org_name



renderView (OrganizationsForums org_name New params)                 = renderView_Forums_NewS
renderView (OrganizationsForums org_name (Edit forum_name) params)   = renderView_Forums_EditS forum_name
renderView (OrganizationsForums org_name (Delete forum_name) params) = renderView_Forums_DeleteS forum_name
renderView (OrganizationsForums org_name (Show forum_name) params)   = renderView_Organizations_Forums_Show forum_name



renderView (OrganizationsForumsBoards org_name forum_name New params)                 =
  renderView_Boards_NewS
renderView (OrganizationsForumsBoards org_name forum_name (Edit board_name) params)   =
  renderView_Boards_EditS board_name
renderView (OrganizationsForumsBoards org_name forum_name (Delete board_name) params) =
  renderView_Boards_DeleteS board_name
renderView (OrganizationsForumsBoards org_name forum_name (Show board_name) params)   =
  renderView_Organizations_Forums_Boards_Show board_name



renderView (OrganizationsForumsBoardsThreads org_name forum_name board_name New params)                  =
  renderView_Threads_NewS
renderView (OrganizationsForumsBoardsThreads org_name forum_name board_name (Edit thread_name) params)   =
  renderView_Threads_EditS thread_name
renderView (OrganizationsForumsBoardsThreads org_name forum_name board_name (Delete thread_name) params) =
  renderView_Threads_DeleteS thread_name
renderView (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread_name) params)   =
  renderView_Organizations_Forums_Boards_Threads_Show thread_name



renderView (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name New params)               =
  renderView_ThreadPosts_New
renderView (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (EditI post_id) params)   =
  renderView_ThreadPosts_Edit post_id
renderView (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (DeleteI post_id) params) =
  renderView_ThreadPosts_Delete post_id
renderView (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (ShowI post_id) params)   =
  renderView_ThreadPosts_Show



renderView (Resources Index params )                = renderView_Resources_Index
renderView (Resources New params)                   = renderView_Resources_New
renderView (Resources (EditI resource_id) params)   = renderView_Resources_Edit resource_id
renderView (Resources (ShowI resource_id) params)   = renderView_Resources_Show resource_id
renderView (Resources (DeleteI resource_id) params) = renderView_Resources_Delete resource_id



renderView (ResourcesLeurons resource_id Index params)                    = renderView_Resources_Leurons_Index resource_id
renderView (ResourcesLeurons resource_id New params)                      = renderView_Leurons_New resource_id
renderView (ResourcesLeurons resource_id (EditI leuron_id) params)        = renderView_Leurons_Edit resource_id leuron_id
renderView (ResourcesLeurons resource_id (ShowI leuron_id) params)        = renderView_Leurons_Show resource_id leuron_id
renderView (ResourcesLeurons resource_id (DeleteI leuron_id) params)      = renderView_Leurons_Delete resource_id leuron_id



renderView (ResourcesSiftLeurons resource_id params)                      = renderView_Resources_SiftLeurons resource_id
renderView (ResourcesSiftLeuronsLinear resource_id Index params)          = renderView_Resources_SiftLeuronsLinear_Index resource_id
renderView (ResourcesSiftLeuronsLinear resource_id (ShowI offset) params) = renderView_Resources_SiftLeuronsLinear_Show resource_id offset
renderView (ResourcesSiftLeuronsRandom resource_id params)                = renderView_Resources_SiftLeuronsRandom resource_id



-- renderView (Leurons Index params)              = renderView_Leurons_Index
-- renderView (Leurons (ShowI resource_id) params) = renderView_Leurons_Show resource_id



renderView _ = const $ renderView_404



errs :: forall a b. Maybe (Array String) -> HTML a b
errs Nothing = H.div_ []
errs (Just errors) = H.div_ (map (\e -> H.p_ [H.text e]) errors)
