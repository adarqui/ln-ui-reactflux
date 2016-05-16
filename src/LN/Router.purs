module Router (
  routing,
  routeSignal,
  redirects
) where



import Control.Alt             ((<|>))
import Control.Apply           ((*>))
import Control.Plus            (empty)
import Daimyo.Data.ArrayList   (listToArray)
import Data.Functor            ((<$))
import Data.List               (List(..))
import Data.Map                as M
import Data.Maybe              (Maybe)
import Data.String             (length)
import Data.Tuple              (Tuple(..), uncurry)
import Halogen                 hiding (set)
import Prelude                 (Unit, bind, pure, (<$>), (<*>), ($), (<<<), (>))
import Routing                 (matchesAff)
import Routing.Match           (Match(..))
import Routing.Match.Class     (class MatchClass, lit, str, params)
import Routing.Types           (RoutePart(..))

import LN.Input.Types          (Input(..))
import LN.Router.Types         (Routing, Routes(..), CRUD(..))



-- params' :: forall f. (Bind f, MatchClass f) => f (Array (Tuple String String))
params' :: forall f. MatchClass f => f (Array (Tuple String String))
params' =
  (listToArray <<< M.toList) <$> params




-- | Matches a non-empty string
--
str1 :: Match String
str1 = Match \route ->
    case route of
      Cons (Path input) rs ->
        if length input > 0
          then pure $ Tuple rs input
          else empty
      _ -> empty
--      _ -> mempty
--      _ ->
--        fail "ExpectedString"
        -- invalid $ free ExpectedString




routing :: Match Routes
routing =
      about <|>
      portal_users <|>
      portal <|>
      users_profile <|>
      users_settings <|>
      users_pms <|>
      users_threads <|>
      users_thread_posts <|>
      users_workouts <|>
      users_resources <|>
      users_leurons <|>
      users_likes <|>
      users <|>
      me <|>
      resources_new <|>
      resources_show <|>
      resources_index <|>
      login <|>
      logout <|>
      organizations_forums_boards_threads <|>
      organizations_forums_boards <|>
      organizations_forums <|>
      organizations_new <|>
      organizations_index <|>
      organizations_show <|>
      home <|>
      home2
  where
    about = About <$ route "about"
    me = Me <$ route "me"
    home = Home <$ lit ""
    home2 = pure Home

    portal_users = PortalUsers <$> (lit "" *> lit "portal" *> lit "users" *> (params' <|> pure []))
    portal = Portal <$ route "portal"

    users_profile =
      UsersProfile <$> (lit "" *> lit "u" *> str) <*> (lit "profile" *> (params' <|> pure []))

    users_settings =
      UsersSettings <$> (lit "" *> lit "u" *> str) <*> (lit "settings" *> (params' <|> pure []))

    users_pms =
      UsersPMs <$> (lit "" *> lit "u" *> str) <*> (lit "pms" *> (params' <|> pure []))

    users_threads =
      UsersThreads <$> (lit "" *> lit "u" *> str) <*> (lit "threads" *> (params' <|> pure []))

    users_thread_posts =
      UsersThreadPosts <$> (lit "" *> lit "u" *> str) <*> (lit "thread_posts" *> (params' <|> pure []))

    users_workouts =
      UsersWorkouts <$> (lit "" *> lit "u" *> str) <*> (lit "workouts" *> (params' <|> pure []))

    users_resources =
      UsersResources <$> (lit "" *> lit "u" *> str) <*> (lit "resources" *> (params' <|> pure []))

    users_leurons =
      UsersLeurons <$> (lit "" *> lit "u" *> str) <*> (lit "leurons" *> (params' <|> pure []))

    users_likes =
      UsersLikes <$> (lit "" *> lit "u" *> str) <*> (lit "likes" *> (params' <|> pure []))

    users =
      Users
      <$> (lit "" *> lit "u" *> (Show <$> str))

    organizations_index =
      Organizations
      <$> (lit "" *> lit "organizations" *> pure Index)
      <*> (params' <|> pure [])

    organizations_new =
      Organizations
      <$> (lit "" *> lit "organizations" *> lit "new" *> pure New)
      <*> (params' <|> pure [])

    organizations_show =
      Organizations
      <$> (lit "" *> (Show <$> str1))
      <*> (params' <|> pure [])

    -- orgname/f/forumname
    organizations_forums =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> (Show <$> str))
      <*> (params' <|> pure [])

    -- orgname/f/forumname/boardname
    organizations_forums_boards =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str)
      <*> (lit "b" *> (Show <$> str))
--      <*> (Show <$> str)
      <*> (params' <|> pure [])

    -- orgname/f/forumname/boardname
    organizations_forums_boards_threads =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str)
      <*> (lit "b" *> str)
      <*> (lit "t" *> (Show <$> str))
      <*> (params' <|> pure [])

    resources_index =
      Resources
      <$> (lit "" *> lit "resources" *> pure Index)
      <*> (params' <|> pure [])

    resources_new =
      Resources
      <$> (lit "" *> lit "resources" *> lit "new" *> pure New)
      <*> (params' <|> pure [])

    resources_show =
      Resources
      <$> (lit "" *> lit "resources" *> (Show <$> str1))
      <*> (params' <|> pure [])

    login = Login <$ route "login"
    logout = Logout <$ route "logout"

    route str = lit "" *> lit str

{-
    parseCRUD =
      Show
      <$> str
      <|> New <$ lit "new"
      <|> pure Index
      -}



routeSignal :: forall eff. Driver Input eff -> Routing eff Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  uncurry (redirects driver) (Tuple old new)



redirects :: forall eff. Driver Input eff -> Maybe Routes -> Routes -> Routing eff Unit
redirects driver _ =
  driver <<< action <<< Goto
