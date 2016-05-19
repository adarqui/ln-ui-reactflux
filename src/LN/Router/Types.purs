module LN.Router.Types (
  Routing,
  CRUD (..),
  Params (..),
  Routes (..),
  class HasLink,
  link,
  class HasCrumb,
  crumb,
  class HasOrderBy,
  orderBy,
  links
) where



import Control.Monad.Aff           (Aff())
import Control.Monad.Aff.AVar      (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Generic                (class Generic, gEq, gShow)
import Data.Map                    as M
import Data.Tuple                  (Tuple(..), fst, snd)
import DOM                         (DOM())
import LN.Router.Util              (slash, fixParams)
import LN.T                        (OrderBy(..))
import Prelude                     (class Eq, class Show, show, (<>), ($), (++), (==))



type Routing e = Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)



data CRUD
  = Index
  | Show String
  | New



instance eqCrud :: Eq CRUD where
  eq Index Index       = true
  eq New New           = true
  eq (Show a) (Show b) = a == b
  eq _ _               = false



type Params = Array (Tuple String String)



data Routes
  = Home
  | About
  | Me
  | Portal
  | Organizations CRUD Params
  | OrganizationsForums String CRUD Params
  | OrganizationsForumsBoards String String CRUD Params
  | OrganizationsForumsBoardsThreads String String String CRUD Params
  | OrganizationsForumsBoardsThreadsPosts String String String String CRUD
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
  | ResourcesSiftLeurons Int CRUD Params
  | Leurons CRUD Params
  | Login
  | Logout
  | NotFound



derive instance genericRoutes :: Generic Routes
derive instance genericCrud :: Generic CRUD



instance eqRoute :: Eq Routes where eq = gEq



class HasLink a where
  link :: a -> Tuple String (M.Map String String)



instance routesHasLink :: HasLink Routes where
  link Home = Tuple "#/" M.empty
  link About = Tuple "#/about" M.empty

  link Me = Tuple "#/me" M.empty

  link Portal = Tuple "#/portal" M.empty

  link (Organizations Index params) = Tuple "#/organizations" (fixParams params)
  link (Organizations crud params) = Tuple ("#" ++ (fst $ link crud)) (fixParams params)

  link (OrganizationsForums org crud params) = Tuple ("#/" <> org <> "/f" <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoards org forum crud params) = Tuple ("#/" <> org <> "/f/" <> forum <> "/b" <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoardsThreads org forum board crud params) = Tuple ("#/" <> org <> "/f/" <> forum <> "/b/" <> board <> "/t" <> (fst $ link crud)) (fixParams params)

  link (OrganizationsForumsBoardsThreadsPosts org forum board thread crud) = Tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> (fst $ link crud)) M.empty

  link (Users Index params) = Tuple "#/u" (fixParams params)
  link (Users crud params) = Tuple ("#/u" ++ (fst $ link crud)) (fixParams params)
  link (UsersProfile user params) = Tuple ("#/u/" <> user <> "/profile") (fixParams params)
  link (UsersSettings user params) = Tuple ("#/u/" <> user <> "/settings") (fixParams params)
  link (UsersPMs user params) = Tuple ("#/u/" <> user <> "/pms") (fixParams params)
  link (UsersThreads user params) = Tuple ("#/u/" <> user <> "/threads") (fixParams params)
  link (UsersThreadPosts user params) = Tuple ("#/u/" <> user <> "/thread_posts") (fixParams params)
  link (UsersWorkouts user params) = Tuple ("#/u/" <> user <> "/workouts") (fixParams params)
  link (UsersResources user params) = Tuple ("#/u/" <> user <> "/resources") (fixParams params)
  link (UsersLeurons user params) = Tuple ("#/u/" <> user <> "/leurons") (fixParams params)
  link (UsersLikes user params) = Tuple ("#/u/" <> user <> "/likes") (fixParams params)

  link (Resources crud params) = Tuple ("#/resources" ++ (fst $ link crud)) (fixParams params)
  link (ResourcesLeurons resource_id crud params) = Tuple ("#/resources/" <> show resource_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeurons resource_id crud params) = Tuple ("#/resources/" <> show resource_id <> "/sifting" <> (fst $ link crud)) (fixParams params)
  link (Leurons crud params) = Tuple ("#/leurons" ++ (fst $ link crud)) (fixParams params)
  link Login = Tuple "/auth/login" M.empty
  link Logout = Tuple "/auth/logout" M.empty
  link NotFound = Tuple "#/404" M.empty
--     link _ = Tuple "#/404" M.empty




class HasCrumb a where
  crumb :: a -> Array (Tuple Routes String)



instance routesHasCrumb :: HasCrumb Routes where
  crumb Home = [Tuple Home "Home"]
  crumb About = [Tuple About "About"]
  crumb Me = [Tuple Me "Me"]
  crumb Portal = [Tuple Portal "Portal"]

  crumb (Organizations Index params) =
    [ Tuple (Organizations Index params) "Organizations" ]

  crumb (Organizations (Show org) params) =
    [
      Tuple (Organizations Index params) "Organizations",
      Tuple (Organizations (Show $ slash org) params) org
    ]

  crumb (OrganizationsForums org (Show forum) params) =
    [
      Tuple (Organizations Index params) "Organizations",
      Tuple (Organizations (Show $ slash org) params) org,
      Tuple (OrganizationsForums org (Show $ slash forum) params) forum
    ]

  crumb (OrganizationsForumsBoards org forum (Show board) params) =
    [
      Tuple (Organizations Index params) "Organizations",
      Tuple (Organizations (Show $ slash org) params) org,
      Tuple (OrganizationsForums org (Show $ slash forum) []) forum,
      Tuple (OrganizationsForumsBoards org forum (Show $ slash board) params) board
    ]

  crumb (OrganizationsForumsBoardsThreads org forum board (Show thread) params) =
    [
      Tuple (Organizations Index params) "Organizations",
      Tuple (Organizations (Show $ slash org) params) org,
      Tuple (OrganizationsForums org (Show $ slash forum) []) forum,
      Tuple (OrganizationsForumsBoards org forum (Show $ slash board) []) board,
      Tuple (OrganizationsForumsBoardsThreads org forum board (Show $ slash thread) params) thread
    ]

  crumb (Users Index params) =
    [
      Tuple (Users Index params) "Users"
    ]

  crumb (Users (Show user) params) =
    [
      Tuple (Users Index params) "Users",
      Tuple (Users (Show $ slash user) params) user
    ]

  crumb (UsersProfile user params) =
    [
      Tuple (Users Index params) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersProfile (slash user) params) "Profile"
    ]

  crumb (UsersSettings user params) =
    [
      Tuple (Users Index params) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersSettings (slash user) params) "Settings"
    ]

  crumb (UsersPMs user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersPMs (slash user) params) "PMs"
    ]

  crumb (UsersThreads user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersThreads (slash user) params) "Threads"
    ]

  crumb (UsersThreadPosts user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersThreadPosts (slash user) params) "ThreadPosts"
    ]

  crumb (UsersWorkouts user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersWorkouts (slash user) params) "Workouts"
    ]

  crumb (UsersResources user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersResources (slash user) params) "Resources"
    ]

  crumb (UsersLeurons user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersLeurons (slash user) params) "Leurons"
    ]

  crumb (UsersLikes user params) =
    [
      Tuple (Users Index []) "Users",
      Tuple (Users (Show $ slash user) []) user,
      Tuple (UsersLikes (slash user) params) "Likes"
    ]

  crumb (Resources Index params) =
    [Tuple (Resources Index params) "Resources"]

  crumb (Resources (Show resource_id) params) =
    [
      Tuple (Resources Index params) "Resources",
      Tuple (Resources (Show $ slash resource_id) params) resource_id
    ]

  crumb (ResourcesLeurons resource_id Index params) =
    [
      Tuple (Resources Index params) "Resources",
      Tuple (Resources (Show $ slash $ show resource_id) params) (show resource_id),
      Tuple (ResourcesLeurons resource_id Index params) "Leurons"
    ]

  crumb (ResourcesLeurons resource_id (Show leuron_id) params) =
    [
      Tuple (Resources Index params) "Resources",
      Tuple (Resources (Show $ slash $ show resource_id) params) (show resource_id),
      Tuple (ResourcesLeurons resource_id Index params) "Leurons"
    ]

  crumb _ = [Tuple NotFound "Error"]




class HasOrderBy a where
  orderBy :: a -> Array OrderBy



instance routesHasOrderBy :: HasOrderBy Routes where
  orderBy (OrganizationsForumsBoards org forum (Show board) params) = [OrderBy_CreatedAt, OrderBy_ActivityAt]
  orderBy _                   = []



instance routesShow :: Show Routes where
  show Home = "#/"
  show About = "#/about"
  show Me = "#/me"
  show Portal = "#/portal"
  show (Organizations crud params) = "#/organizations/..."
  show (OrganizationsForums org crud params) = "#/" <> org <> "/f/..."
  show (OrganizationsForumsBoards org forum crud params) = "#/" <> org <> "/f/" <> forum <> "/b/" <> "..."
  show (OrganizationsForumsBoardsThreads org forum board crud params) = "#/" <> org <> "/f/" <> forum <> "/b/" <> board <> "/t/" <> "..."
  show (OrganizationsForumsBoardsThreadsPosts org forum board thread crud) = "#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> "/..."
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
  show (ResourcesSiftLeurons resource_id crud params) = "#/resources/" <> show resource_id <> "/sift/..."
  show (Leurons crud params) = "#/leurons/..."
  show Login = "/auth/login"
  show Logout = "/auth/logout"
  show NotFound = "#/404"



instance crudHasLink :: HasLink CRUD where
-- TODO FIXME:
-- well this could be fixed.. changed from "" in order to match CRUD Index routes
  link Index    = Tuple "" M.empty
--  link Index    = Tuple "/index" M.empty
  link New      = Tuple "/new" M.empty
  link (Show s) = Tuple ("/" <> s) M.empty



links :: Array Routes
links =
  [ Home
  , About
  , Portal

  , Me

  , Organizations Index []
  , OrganizationsForums "adarq" Index []
  , OrganizationsForumsBoards "adarq" "forum" Index []
  , OrganizationsForumsBoardsThreads "adarq" "forum" "board" Index []
  , OrganizationsForumsBoardsThreadsPosts "adarq" "forum" "board" "thread" Index

  , Users Index []
  , UsersProfile "adarq" []
  , UsersSettings "adarq" []
  , UsersPMs "adarq" []
  , UsersThreads "adarq" []
  , UsersThreadPosts "adarq" []
  , UsersWorkouts "adarq" []
  , UsersResources "adarq" []
  , UsersLeurons "adarq" []
  , UsersLikes "adarq" []

  , Resources Index []
  , ResourcesLeurons 1 Index []
  , ResourcesSiftLeurons 1 Index []
  , Leurons Index []
  , Login
  , Logout
  ]
