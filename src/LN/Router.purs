module Router (
  routing,
  routeSignal,
  redirects
) where



import Control.Alt             ((<|>))
import Control.Apply           ((*>), (<*))
import Control.Plus            (empty)
import Daimyo.Data.ArrayList   (listToArray)
import Data.Functor            ((<$))
import Data.Int                (fromString)
import Data.List               (List(..))
import Data.Map                as M
import Data.Maybe              (Maybe(..))
import Data.String             (length)
import Data.Tuple              (Tuple(..), uncurry)
import Halogen                 hiding (set)
import Prelude                 (Unit, bind, pure, const, (<$>), (<*>), ($), (<<<), (>))
import Routing                 (matchesAff)
import Routing.Match           (Match(..))
import Routing.Match.Class     (class MatchClass, lit, str, params)
import Routing.Types           (RoutePart(..))

import LN.Input.Types          (Input(..))
import LN.Router.Types         (Routing, Routes(..), CRUD(..))
import LN.Router.Class.Params  (Params, emptyParams, psRoutingParamsToParams)



-- params' :: forall f. (Bind f, MatchClass f) => f (Array (Tuple String String))
-- params' :: forall f. MatchClass f => f (Array (Tuple String String))
params' :: forall f. MatchClass f => f Params
params' = psRoutingParamsToParams <$> params
--  pure emptyParams
--  (listToArray <<< M.toList) <$> params




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



int = Match \route ->
  case route of
    Cons (Path input) rs ->
      case fromString input of
        Nothing -> empty
        Just n  -> pure $ Tuple rs n
    _ -> empty



routing :: Match Routes
routing =

      about <|>

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

      users_new <|>
      users_show <|>
      users_index <|>

      me <|>

      errors <|>

      resources_sift_linear_show_int <|>
      resources_sift_linear_index <|>
      resources_sift_random <|>

      resources_sift <|>

      resources_leurons_new <|>
      resources_leurons_edit_int <|>
      resources_leurons_delete_int <|>
      resources_leurons_show_int <|>
      resources_leurons_index <|>

      resources_new <|>
      resources_show_int <|>
      resources_edit_int <|>
      resources_delete_int <|>
      resources_index <|>

--      leurons_new <|>
--      leurons_show_int <|>
--      leurons_index <|>

      login <|>
      logout <|>

      -- wtf? why do I have to put this here..
      organizations_forums_boards_threads_posts_show_int <|>

      organizations_new <|>
      organizations_edit <|>
      organizations_delete <|>
      organizations_index <|>

      organizations_forums_boards_threads_posts_new <|>
      organizations_forums_boards_threads_new <|>
      organizations_forums_boards_new <|>
      organizations_forums_new <|>

      organizations_forums_boards_threads_posts_edit_int <|>
      organizations_forums_boards_threads_edit <|>
      organizations_forums_boards_edit <|>
      organizations_forums_edit <|>

      organizations_forums_boards_threads_posts_delete_int <|>
      organizations_forums_boards_threads_delete <|>
      organizations_forums_boards_delete <|>
      organizations_forums_delete <|>

      organizations_forums_boards_threads_posts_index <|>
      organizations_forums_boards_threads_index <|>
      organizations_forums_boards_index <|>
      organizations_forums_index <|>

      organizations_forums_boards_threads_show <|>
      organizations_forums_boards_show <|>
      organizations_forums_show <|>
      organizations_show <|>

      home <|>
      home2
  where

    about = About <$ route "about"



    me = Me <$ route "me"



    errors = Errors <$ route "errors"



    home = Home <$ lit ""
    home2 = pure Home



    portal = Portal <$ route "portal"



    users_profile =
      UsersProfile <$> (lit "" *> lit "u" *> str) <*> (lit "profile" *> (params' <|> pure emptyParams))

    users_settings =
      UsersSettings <$> (lit "" *> lit "u" *> str) <*> (lit "settings" *> (params' <|> pure emptyParams))

    users_pms =
      UsersPMs <$> (lit "" *> lit "u" *> str) <*> (lit "pms" *> (params' <|> pure emptyParams))

    users_threads =
      UsersThreads <$> (lit "" *> lit "u" *> str) <*> (lit "threads" *> (params' <|> pure emptyParams))

    users_thread_posts =
      UsersThreadPosts <$> (lit "" *> lit "u" *> str) <*> (lit "thread_posts" *> (params' <|> pure emptyParams))

    users_workouts =
      UsersWorkouts <$> (lit "" *> lit "u" *> str) <*> (lit "workouts" *> (params' <|> pure emptyParams))

    users_resources =
      UsersResources <$> (lit "" *> lit "u" *> str) <*> (lit "resources" *> (params' <|> pure emptyParams))

    users_leurons =
      UsersLeurons <$> (lit "" *> lit "u" *> str) <*> (lit "leurons" *> (params' <|> pure emptyParams))

    users_likes =
      UsersLikes <$> (lit "" *> lit "u" *> str) <*> (lit "likes" *> (params' <|> pure emptyParams))



    users_index =
      Users
      <$> (lit "" *> lit "u" *> pure Index)
      <*> (params' <|> pure emptyParams)

    users_new =
      Users
      <$> (lit "" *> lit "u" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    users_show =
      Users
      <$> (lit "" *> lit "u" *> (Show <$> str1))
      <*> (params' <|> pure emptyParams)



    organizations_index =
      Organizations
      <$> (lit "" *> lit "organizations" *> pure Index)
      <*> (params' <|> pure emptyParams)

    organizations_new =
      Organizations
      <$> (lit "" *> lit "organizations" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    organizations_edit =
      Organizations
      <$> (lit "" *> lit "organizations" *> lit "_edit" *> (Edit <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_delete =
      Organizations
      <$> (lit "" *> lit "organizations" *> lit "_delete" *> (Delete <$> str1))
      <*> (params' <|> pure emptyParams)




    organizations_forums_new =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_new =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> (lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_new =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> (lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_posts_new =
      OrganizationsForumsBoardsThreadsPosts
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> str1
      <*> (lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)



    organizations_forums_edit =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> lit "_edit" *> (Edit <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_edit =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> (lit "_edit" *> (Edit <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_edit =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> (lit "_edit" *> (Edit <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_posts_edit_int =
      OrganizationsForumsBoardsThreadsPosts
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> str1
      <*> (lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)



    organizations_forums_delete =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> lit "_delete" *> (Delete <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_delete =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> (lit "_delete" *> (Delete <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_delete =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> (lit "_delete" *> (Delete <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_posts_delete_int =
      OrganizationsForumsBoardsThreadsPosts
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> str1
      <*> (lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)



    organizations_forums_index =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> pure Index)
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_index =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> pure Index
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_index =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> pure Index
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_posts_index =
      OrganizationsForumsBoardsThreadsPosts
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> str1
      <*> pure Index
      <*> (params' <|> pure emptyParams)



    organizations_show =
      Organizations
      <$> (lit "" *> (Show <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_show =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> (Show <$> str1))
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_show =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> (Show <$> str1)
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_show =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> (Show <$> str1)
      <*> (params' <|> pure emptyParams)

    organizations_forums_boards_threads_posts_show_int =
      OrganizationsForumsBoardsThreadsPosts
      <$> (lit "" *> str)
      <*> (lit "f" *> str1)
      <*> str1
      <*> str1
      <*> (ShowI <$> int)
      <*> (params' <|> pure emptyParams)



    resources_leurons_index =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> pure Index)
      <*> (params' <|> pure emptyParams)

    resources_leurons_new =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    resources_leurons_edit_int =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_leurons_delete_int =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_leurons_show_int =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)



    resources_sift =
      ResourcesSiftLeurons
      <$> (lit "" *> lit "resources" *> int <* lit "sift")
      <*> (params' <|> pure emptyParams)

    resources_sift_linear_index =
      ResourcesSiftLeuronsLinear
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "sift" *> lit "linear" *> pure Index)
      <*> (params' <|> pure emptyParams)

    resources_sift_linear_show_int =
      ResourcesSiftLeuronsLinear
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "sift" *> lit "linear" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_sift_random =
      ResourcesSiftLeuronsRandom
      <$> (lit "" *> lit "resources" *> int <* lit "sift" <* lit "random")
      <*> (params' <|> pure emptyParams)



    resources_index =
      Resources
      <$> (lit "" *> lit "resources" *> pure Index)
      <*> (params' <|> pure emptyParams)

    resources_new =
      Resources
      <$> (lit "" *> lit "resources" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    resources_show_int =
      Resources
      <$> (lit "" *> lit "resources" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_edit_int =
      Resources
      <$> (lit "" *> lit "resources" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_delete_int =
      Resources
      <$> (lit "" *> lit "resources" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)



--    leurons_index =
--      Leurons
--      <$> (lit "" *> lit "leurons" *> pure Index)
--      <*> (params' <|> pure emptyParams)
--
--    leurons_new =
--      Leurons
--      <$> (lit "" *> lit "leurons" *> lit "new" *> pure New)
--      <*> (params' <|> pure emptyParams)
--
--    leurons_show_int =
--      Leurons
--      <$> (lit "" *> lit "leurons" *> (ShowI <$> int))
--      <*> (params' <|> pure emptyParams)



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
