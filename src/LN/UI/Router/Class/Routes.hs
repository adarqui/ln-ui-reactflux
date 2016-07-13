module LN.UI.Router.Class.Routes (
  Routes (..),
  HasCrumb,
  crumb
) where



import Data.Map                    as M
import Data.Maybe                  (maybe)
import Data.Tuple                  (fst)
import Data.Monoid ((<>))
import Prelude                     (Eq, Show, show, ($), (++), (==))

import LN.T
import LN.UI.Router.Util              (slash)
import LN.UI.Router.Class.CRUD
import LN.UI.Router.Class.Params      (Params, emptyParams, fixParams)
import LN.UI.Router.Class.Link
import LN.UI.Router.Class.OrderBy
import LN.UI.State.Internal.Types     (InternalState)
import LN.UI.Types (Array, String, Int, Tuple, tuple)



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
  | OrganizationsTeams String CRUD Params
  | OrganizationsTeamsMembers String String CRUD Params
  | OrganizationsMembersOnly String
  | OrganizationsMembership String CRUD Params
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




class HasCrumb a where
  crumb :: a -> InternalState Routes -> Array (Tuple Routes String)



instance HasLink Routes where

  link Home   = tuple "#/"       emptyParams

  link About  = tuple "#/about"  emptyParams

  link Me     = tuple "#/me"     emptyParams

  link Errors = tuple "#/errors" emptyParams

  link Portal = tuple "#/portal" emptyParams

  link (Organizations Index params)                  = tuple "#/organizations" (fixParams params)
  link (Organizations crud@(New) params)             = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  link (Organizations crud@(Edit org_name) params)   = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  link (Organizations crud@(Delete org_name) params) = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  link (Organizations crud@(Show org_name) params)   = tuple ("#" ++ (fst $ link crud)) (fixParams params)

  link (OrganizationsForums org Index params) =
    tuple ("#/" <> org <> "/f") (fixParams params)
  link (OrganizationsForums org crud params) =
    tuple ("#/" <> org <> "/f" <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoards org forum crud params) =
    tuple ("#/" <> org <> "/f/" <> forum <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoardsThreads org forum board crud params) =
    tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) =
    tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> (fst $ link crud)) (fixParams params)

  link (OrganizationsTeams org crud params) =
    tuple ("#/" <> org <> "/teams" <> (fst $ link crud)) (fixParams params)

  link (OrganizationsTeamsMembers org team crud params) =
    tuple ("#/" <> org <> "/teams/" <> team <> (fst $ link crud)) (fixParams params)

  link (OrganizationsMembersOnly org) =
    tuple ("#/" <> org <> "/_members_only") emptyParams

  link (OrganizationsMembership org crud params) =
    tuple ("#/" <> org <> "/membership" <> (fst $ link crud)) (fixParams params)

  link (Users Index params)           = tuple "#/u" (fixParams params)
  link (Users crud params)            = tuple ("#/u" ++ (fst $ link crud)) (fixParams params)
  link (UsersProfile user params)     = tuple ("#/u/" <> user <> "/profile") (fixParams params)
  link (UsersSettings user params)    = tuple ("#/u/" <> user <> "/settings") (fixParams params)
  link (UsersPMs user params)         = tuple ("#/u/" <> user <> "/pms") (fixParams params)
  link (UsersThreads user params)     = tuple ("#/u/" <> user <> "/threads") (fixParams params)
  link (UsersThreadPosts user params) = tuple ("#/u/" <> user <> "/thread_posts") (fixParams params)
  link (UsersWorkouts user params)    = tuple ("#/u/" <> user <> "/workouts") (fixParams params)
  link (UsersResources user params)   = tuple ("#/u/" <> user <> "/resources") (fixParams params)
  link (UsersLeurons user params)     = tuple ("#/u/" <> user <> "/leurons") (fixParams params)
  link (UsersLikes user params)       = tuple ("#/u/" <> user <> "/likes") (fixParams params)

  link (Resources crud params)                              = tuple ("#/resources" ++ (fst $ link crud)) (fixParams params)

  link (ResourcesLeurons resource_id crud params)           = tuple ("#/resources/" <> show resource_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeurons resource_id params) = tuple ("#/resources/" <> show resource_id <> "/sift") (fixParams params)
  link (ResourcesSiftLeuronsLinear resource_id crud params) = tuple ("#/resources/" <> show resource_id <> "/sift/linear" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeuronsRandom resource_id params)      = tuple ("#/resources/" <> show resource_id <> "/sift/random") (fixParams params)

--  link (Leurons crud params) = tuple ("#/leurons" ++ (fst $ link crud)) (fixParams params)

  link Login    = tuple "/auth/login" emptyParams
  link Logout   = tuple "/auth/logout" emptyParams

  link NotFound = tuple "#/404" emptyParams




instance HasCrumb Routes where

  crumb route st =

    case route of



      Home   -> [tuple Home "Home"]



      About  -> [tuple About "About"]



      Me     -> [tuple Me "Me"]



      Errors -> [tuple Errors "Errors"]



      Portal -> [tuple Portal "Portal"]



      Organizations Index params ->
        [
          tuple (Organizations Index params) "Organizations"
        ]

      Organizations New params ->
        [
          tuple (Organizations Index emptyParams) "Organizations"
        ]

      Organizations (Edit org_name) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org_name) emptyParams) org_name
        ]

      Organizations (Delete org_name) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org_name) emptyParams) org_name
        ]

      Organizations (Show org) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) params) org
        ]



      OrganizationsForums org Index params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org
        ]

      OrganizationsForums org New params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org
        ]

      OrganizationsForums org (Edit forum) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum
        ]

      OrganizationsForums org (Delete forum) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum
        ]

      OrganizationsForums org (Show forum) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) params) forum
        ]



      OrganizationsForumsBoards org forum Index params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum
        ]

      OrganizationsForumsBoards org forum New params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum
        ]

      OrganizationsForumsBoards org forum (Edit board) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
        ]

      OrganizationsForumsBoards org forum (Delete board) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
        ]

      OrganizationsForumsBoards org forum (Show board) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) params) board
        ]



      OrganizationsForumsBoardsThreads org forum board Index params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
        ]

      OrganizationsForumsBoardsThreads org forum board New params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
        ]

      OrganizationsForumsBoardsThreads org forum board (Edit thread) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
        ]

      OrganizationsForumsBoardsThreads org forum board (Delete thread) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
        ]

      OrganizationsForumsBoardsThreads org forum board (Show thread) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread
        ]



      OrganizationsForumsBoardsThreadsPosts org forum board thread Index params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread New params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread (EditI post) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread,
          tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) emptyParams) (show post)
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread (DeleteI post) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread,
          tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) emptyParams) (show post)
        ]

      OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params ->
        [
          tuple (Organizations Index emptyParams) "Organizations",
          tuple (Organizations (Show org) emptyParams) org,
          tuple (OrganizationsForums org (Show forum) emptyParams) forum,
          tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
          tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread,
          tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params) (show post)
        ]



      Users Index params ->
        [
          tuple (Users Index params) "Users"
        ]

      Users (Show user) params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) params) user
        ]



      UsersProfile user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersProfile (slash user) params) "Profile"
        ]

      UsersSettings user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersSettings (slash user) params) "Settings"
        ]

      UsersPMs user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersPMs (slash user) params) "PMs"
        ]

      UsersThreads user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersThreads (slash user) params) "Threads"
        ]

      UsersThreadPosts user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersThreadPosts (slash user) params) "ThreadPosts"
        ]

      UsersWorkouts user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersWorkouts (slash user) params) "Workouts"
        ]

      UsersResources user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersResources (slash user) params) "Resources"
        ]

      UsersLeurons user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersLeurons (slash user) params) "Leurons"
        ]

      UsersLikes user params ->
        [
          tuple (Users Index emptyParams) "Users",
          tuple (Users (Show user) emptyParams) user,
          tuple (UsersLikes (slash user) params) "Likes"
        ]



      Resources Index params ->
        [tuple (Resources Index params) "Resources"]

      Resources New params ->
        [tuple (Resources Index params) "Resources"]

      Resources (EditI resource_id) params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id params
        ]

      Resources (DeleteI resource_id) params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id params
        ]

      Resources (ShowI resource_id) params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id params
        ]



      ResourcesLeurons resource_id Index params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesLeurons resource_id Index params) "Leurons"
        ]

      ResourcesLeurons resource_id New params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesLeurons resource_id Index params) "Leurons"
        ]

      ResourcesLeurons resource_id (EditI leuron_id) params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
          tuple (ResourcesLeurons resource_id (ShowI leuron_id) emptyParams) (show leuron_id)
        ]

      ResourcesLeurons resource_id (DeleteI leuron_id) params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
          tuple (ResourcesLeurons resource_id (ShowI leuron_id) emptyParams) (show leuron_id)
        ]

      ResourcesLeurons resource_id (ShowI leuron_id) params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
          tuple (ResourcesLeurons resource_id (ShowI leuron_id) params) (show leuron_id)
        ]



      ResourcesSiftLeurons resource_id params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesSiftLeurons resource_id params) "Sift"
        ]

      ResourcesSiftLeuronsRandom resource_id params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesSiftLeurons resource_id params) "Sift"
        ]

      ResourcesSiftLeuronsLinear resource_id _ params ->
        [
          tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          tuple (ResourcesSiftLeurons resource_id emptyParams) "Sift",
          tuple (ResourcesSiftLeuronsLinear resource_id Index params) "Linear"
        ]


      _ -> [tuple NotFound "Error"]

    where
    resource_pretty resource_id params =
      tuple (Resources (ShowI resource_id) params) ""
--        $ maybe (show resource_id) (\pack -> pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse .. displayName_) st.currentResource




instance HasOrderBy Routes where
  orderBy (OrganizationsForumsBoards org forum (Show board) params) = [OrderBy_CreatedAt, OrderBy_ActivityAt]
  orderBy _                   = []



instance Show Routes where
  show Home   = "Home"
  show About  = "About"
  show Me     = "Me"
  show Errors = "Errors"
  show Portal = "Portal"
  show (Organizations crud params) =
    "Organizations " <> show crud
  show (OrganizationsForums org crud params) =
    "OrganizationsForums " <> org <> sp <> show crud
  show (OrganizationsForumsBoards org forum crud params) =
    "OrganizationsForumsBoards " <> org <> sp <> forum <> sp <> show crud
  show (OrganizationsForumsBoardsThreads org forum board crud params) =
    "OrganizationsForumsBoardsThreads " <> org <> sp <> forum <> sp <> board <> sp <> show crud
  show (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) =
    "OrganizationsForumsBoardsThreads " <> org <> sp <> forum <> sp <> board <> sp <> thread <> sp <> show crud
  show (OrganizationsTeams org crud params) =
    "OrganizationsTeams " <> org <> sp <> show crud
  show (OrganizationsTeamsMembers org team crud params) =
    "OrganizationsTeamsMembers " <> org <> sp <> team <> sp <> show crud
  show (OrganizationsMembersOnly org) =
    "OrganizationsMembersOnly " <> org
  show (OrganizationsMembership org crud params) =
    "OrganizationsMembership " <> org <> sp <> show crud
  show (Users crud params)            = "Users " <> show crud
  show (UsersProfile user params)     = "UsersProfile " <> user
  show (UsersSettings user params)    = "UsersSettings " <> user
  show (UsersPMs user params)         = "UsersPMs " <> user
  show (UsersThreads user params)     = "UsersThreads " <> user
  show (UsersThreadPosts user params) = "UsersThreadPosts " <> user
  show (UsersWorkouts user params)    = "UsersWorkouts " <> user
  show (UsersResources user params)   = "UsersResources " <> user
  show (UsersLeurons user params)     = "UsersLeurons " <> user
  show (UsersLikes user params)       = "UsersLikes " <> user
  show (Resources crud params)        = "Resources " <> show crud
  show (ResourcesLeurons resource_id crud params)           = "ResourcesLeurons " <> show resource_id <> sp <> show crud
  show (ResourcesSiftLeurons resource_id params)            = "ResourcesSiftLeurons " <> show resource_id
  show (ResourcesSiftLeuronsLinear resource_id crud params) = "ResourcesSiftLeuronsLinear " <> show resource_id <> sp <> show crud
  show (ResourcesSiftLeuronsRandom resource_id params)      = "ResourcesSiftLeuronsRandom " <> show resource_id
  show Login    = "Login"
  show Logout   = "Logout"
  show NotFound = "NotFound"
  show _ = "make sure Show covers all Routes"



sp :: String
sp = " "
