module LN.Router.Class.Routes (
  Routes (..),
  class HasCrumb,
  crumb
) where



import Control.Monad.Aff           (Aff())
import Control.Monad.Aff.AVar      (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Generic                (class Generic, gEq)
import Data.Map                    as M
import Data.Maybe                  (maybe)
import Data.Tuple                  (Tuple(..), fst)
import DOM                         (DOM())
import Optic.Core                  ((^.), (..))
import Prelude                     (class Eq, class Show, show, (<>), ($), (++), (==))

import LN.T
import LN.Router.Util              (slash, fixParams)
import LN.Router.Class.CRUD
import LN.Router.Class.Params
import LN.Router.Class.Link
import LN.Router.Class.OrderBy
import LN.State.Internal.Types     (InternalState)
-- import LN.State.Types              (DriverCh)



data Routes
  = Home
  | About
  | Me
  | Errors
  | Portal
  | Organizations CRUD Params
  | OrganizationsForums String CRUD Params
  | OrganizationsForumsBoards String String CRUD Params
  | OrganizationsForumsBoardsThreads String String String CRUD Params
  | OrganizationsForumsBoardsThreadsPosts String String String String CRUD Params
  | Users CRUD Params
  | UsersProfile String Params
  | UsersSettings String Params
  | UsersPMs String Params
  | UsersThreads String Params
  | UsersThreadPosts String Params
  | UsersWorkouts String Params
  | UsersResources String Params
  | UsersLeurons String Params
  | UsersLikes String Params
  | Resources CRUD Params
  | ResourcesLeurons Int CRUD Params
  | ResourcesSiftLeurons Int Params
  | ResourcesSiftLeuronsLinear Int CRUD Params
  | ResourcesSiftLeuronsRandom Int Params
--  | Leurons CRUD Params
  | Login
  | Logout
  | NotFound



derive instance genericRoutes :: Generic Routes



instance eqRoute :: Eq Routes where eq = gEq



class HasCrumb a where
  crumb :: a -> InternalState Routes -> Array (Tuple Routes String)



instance routesHasLink :: HasLink Routes where

  link Home   = Tuple "#/"       M.empty

  link About  = Tuple "#/about"  M.empty

  link Me     = Tuple "#/me"     M.empty

  link Errors = Tuple "#/errors" M.empty

  link Portal = Tuple "#/portal" M.empty

  link (Organizations Index params)                  = Tuple "#/organizations" (fixParams params)
  link (Organizations crud@(New) params)             = Tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  link (Organizations crud@(Edit org_name) params)   = Tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  link (Organizations crud@(Delete org_name) params) = Tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  link (Organizations crud@(Show org_name) params)   = Tuple ("#" ++ (fst $ link crud)) (fixParams params)

  link (OrganizationsForums org Index params) =
    Tuple ("#/" <> org) (fixParams params)
  link (OrganizationsForums org crud params) =
    Tuple ("#/" <> org <> "/f" <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoards org forum crud params) =
    Tuple ("#/" <> org <> "/f/" <> forum <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoardsThreads org forum board crud params) =
    Tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) =
    Tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> (fst $ link crud)) (fixParams params)

  link (Users Index params)           = Tuple "#/u" (fixParams params)
  link (Users crud params)            = Tuple ("#/u" ++ (fst $ link crud)) (fixParams params)
  link (UsersProfile user params)     = Tuple ("#/u/" <> user <> "/profile") (fixParams params)
  link (UsersSettings user params)    = Tuple ("#/u/" <> user <> "/settings") (fixParams params)
  link (UsersPMs user params)         = Tuple ("#/u/" <> user <> "/pms") (fixParams params)
  link (UsersThreads user params)     = Tuple ("#/u/" <> user <> "/threads") (fixParams params)
  link (UsersThreadPosts user params) = Tuple ("#/u/" <> user <> "/thread_posts") (fixParams params)
  link (UsersWorkouts user params)    = Tuple ("#/u/" <> user <> "/workouts") (fixParams params)
  link (UsersResources user params)   = Tuple ("#/u/" <> user <> "/resources") (fixParams params)
  link (UsersLeurons user params)     = Tuple ("#/u/" <> user <> "/leurons") (fixParams params)
  link (UsersLikes user params)       = Tuple ("#/u/" <> user <> "/likes") (fixParams params)

  link (Resources crud params)                              = Tuple ("#/resources" ++ (fst $ link crud)) (fixParams params)

  link (ResourcesLeurons resource_id crud params)           = Tuple ("#/resources/" <> show resource_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeurons resource_id params) = Tuple ("#/resources/" <> show resource_id <> "/sift") (fixParams params)
  link (ResourcesSiftLeuronsLinear resource_id crud params) = Tuple ("#/resources/" <> show resource_id <> "/sift/linear" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeuronsRandom resource_id params)      = Tuple ("#/resources/" <> show resource_id <> "/sift/random") (fixParams params)

--  link (Leurons crud params) = Tuple ("#/leurons" ++ (fst $ link crud)) (fixParams params)

  link Login    = Tuple "/auth/login" M.empty
  link Logout   = Tuple "/auth/logout" M.empty

  link NotFound = Tuple "#/404" M.empty




instance routesHasCrumb :: HasCrumb Routes where

  crumb route st =

    case route of



      Home   -> [Tuple Home "Home"]



      About  -> [Tuple About "About"]



      Me     -> [Tuple Me "Me"]



      Errors -> [Tuple Errors "Errors"]



      Portal -> [Tuple Portal "Portal"]



      Organizations Index params ->
        [
          Tuple (Organizations Index params) "Organizations"
        ]

      Organizations New params ->
        [
          Tuple (Organizations Index params) "Organizations"
        ]

      Organizations (Edit org_name) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org_name) params) org_name
        ]

      Organizations (Delete org_name) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org_name) params) org_name
        ]

      Organizations (Show org) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org
        ]



      OrganizationsForums org Index params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org
        ]

      OrganizationsForums org New params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org
        ]

      OrganizationsForums org (Edit forum) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) params) forum
        ]

      OrganizationsForums org (Delete forum) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) params) forum
        ]

      OrganizationsForums org (Show forum) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) params) forum
        ]



      OrganizationsForumsBoards org forum Index params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum
        ]

      OrganizationsForumsBoards org forum New params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum
        ]

      OrganizationsForumsBoards org forum (Edit board) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) params) board
        ]

      OrganizationsForumsBoards org forum (Delete board) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) params) board
        ]

      OrganizationsForumsBoards org forum (Show board) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) params) board
        ]



      OrganizationsForumsBoardsThreads org forum board Index params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board
        ]

      OrganizationsForumsBoardsThreads org forum board New params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board
        ]

      OrganizationsForumsBoardsThreads org forum board (Edit thread) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread
        ]

      OrganizationsForumsBoardsThreads org forum board (Delete thread) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread
        ]

      OrganizationsForumsBoardsThreads org forum board (Show thread) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread
        ]



      OrganizationsForumsBoardsThreadsPosts org forum board thread Index params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) []) thread
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread New params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) []) thread
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread (EditI post) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread,
          Tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params) (show post)
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread (DeleteI post) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread,
          Tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params) (show post)
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params ->
        [
          Tuple (Organizations Index params) "Organizations",
          Tuple (Organizations (Show org) params) org,
          Tuple (OrganizationsForums org (Show forum) []) forum,
          Tuple (OrganizationsForumsBoards org forum (Show board) []) board,
          Tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread,
          Tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params) (show post)
        ]



      Users Index params ->
        [
          Tuple (Users Index params) "Users"
        ]

      Users (Show user) params ->
        [
          Tuple (Users Index params) "Users",
          Tuple (Users (Show user) params) user
        ]



      UsersProfile user params ->
        [
          Tuple (Users Index params) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersProfile (slash user) params) "Profile"
        ]

      UsersSettings user params ->
        [
          Tuple (Users Index params) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersSettings (slash user) params) "Settings"
        ]

      UsersPMs user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersPMs (slash user) params) "PMs"
        ]

      UsersThreads user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersThreads (slash user) params) "Threads"
        ]

      UsersThreadPosts user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersThreadPosts (slash user) params) "ThreadPosts"
        ]

      UsersWorkouts user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersWorkouts (slash user) params) "Workouts"
        ]

      UsersResources user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersResources (slash user) params) "Resources"
        ]

      UsersLeurons user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersLeurons (slash user) params) "Leurons"
        ]

      UsersLikes user params ->
        [
          Tuple (Users Index []) "Users",
          Tuple (Users (Show user) []) user,
          Tuple (UsersLikes (slash user) params) "Likes"
        ]



      Resources Index params ->
        [Tuple (Resources Index params) "Resources"]

      Resources New params ->
        [Tuple (Resources Index params) "Resources"]

      Resources (EditI resource_id) params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params
        ]

      Resources (DeleteI resource_id) params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params
        ]

      Resources (ShowI resource_id) params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params
        ]



      ResourcesLeurons resource_id Index params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons"
        ]

      ResourcesLeurons resource_id New params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons"
        ]

      ResourcesLeurons resource_id (EditI leuron_id) params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons",
          Tuple (ResourcesLeurons resource_id (ShowI leuron_id) params) (show leuron_id)
        ]

      ResourcesLeurons resource_id (DeleteI leuron_id) params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons",
          Tuple (ResourcesLeurons resource_id (ShowI leuron_id) params) (show leuron_id)
        ]

      ResourcesLeurons resource_id (ShowI leuron_id) params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons",
          Tuple (ResourcesLeurons resource_id (ShowI leuron_id) params) (show leuron_id)
        ]



      ResourcesSiftLeurons resource_id params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesSiftLeurons resource_id params) "Sift"
        ]

      ResourcesSiftLeuronsRandom resource_id params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesSiftLeurons resource_id params) "Sift"
        ]

      ResourcesSiftLeuronsLinear resource_id _ params ->
        [
          Tuple (Resources Index params) "Resources",
          resource_pretty resource_id params,
          Tuple (ResourcesSiftLeurons resource_id params) "Sift",
          Tuple (ResourcesSiftLeuronsLinear resource_id Index params) "Linear"
        ]


      _ -> [Tuple NotFound "Error"]

    where
    resource_pretty resource_id params =
      Tuple (Resources (ShowI resource_id) params)
        $ maybe (show resource_id) (\pack -> pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse .. displayName_) st.currentResource




instance routesHasOrderBy :: HasOrderBy Routes where
  orderBy (OrganizationsForumsBoards org forum (Show board) params) = [OrderBy_CreatedAt, OrderBy_ActivityAt]
  orderBy _                   = []



instance routesShow :: Show Routes where
  show Home = "#/"
  show About = "#/about"
  show Me = "#/me"
  show Errors = "#/errors"
  show Portal = "#/portal"
  show (Organizations crud params) = "#/organizations/..."
  show (OrganizationsForums org crud params) = "#/" <> org <> "/f/..."
  show (OrganizationsForumsBoards org forum crud params) = "#/" <> org <> "/f/" <> forum <> "/b/" <> "..."
  show (OrganizationsForumsBoardsThreads org forum board crud params) = "#/" <> org <> "/f/" <> forum <> "/b/" <> board <> "/t/" <> "..."
  show (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) = "#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> "/..."
  show (Users crud params) = "#/u/..."
  show (UsersProfile user params) = "#/u/profile"
  show (UsersSettings user params) = "#/u/settings"
  show (UsersPMs user params) = "#/u/pms"
  show (UsersThreads user params) = "#/u/threads"
  show (UsersThreadPosts user params) = "#/u/thread_posts"
  show (UsersWorkouts user params) = "#/u/workouts"
  show (UsersResources user params) = "#/u/resources"
  show (UsersLeurons user params) = "#/u/leurons"
  show (UsersLikes user params) = "#/u/likes"
  show (Resources crud params) = "#/resources/..."
  show (ResourcesLeurons resource_id crud params) = "#/resources/" <> show resource_id <> "/leurons/..."
  show (ResourcesSiftLeurons resource_id params) = "#/resources/" <> show resource_id <> "/sift/..."
  show (ResourcesSiftLeuronsLinear resource_id crud params) = "#/resources/" <> show resource_id <> "/sift/linear/..."
  show (ResourcesSiftLeuronsRandom resource_id params) = "#/resources/" <> show resource_id <> "/sift/random"
--  show (Leurons crud params) = "#/leurons/..."
  show Login = "/auth/login"
  show Logout = "/auth/logout"
  show NotFound = "#/404"
