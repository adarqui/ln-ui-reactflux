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
      resources_leurons_index <|>
      resources_leurons_show <|>

      resources_new <|>
      resources_show_int <|>
      resources_edit_int <|>
      resources_delete_int <|>
      resources_index <|>

      leurons_new <|>
      leurons_show_int <|>
      leurons_index <|>

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



    errors = Errors <$ route "errors"



    home = Home <$ lit ""
    home2 = pure Home



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



    users_index =
      Users
      <$> (lit "" *> lit "u" *> pure Index)
      <*> (params' <|> pure [])

    users_new =
      Users
      <$> (lit "" *> lit "u" *> lit "new" *> pure New)
      <*> (params' <|> pure [])

    users_show =
      Users
      <$> (lit "" *> lit "u" *> (Show <$> str1))
      <*> (params' <|> pure [])



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

    organizations_forums =
      OrganizationsForums
      <$> (lit "" *> str)
      <*> (lit "f" *> (Show <$> str))
      <*> (params' <|> pure [])

    organizations_forums_boards =
      OrganizationsForumsBoards
      <$> (lit "" *> str)
      <*> (lit "f" *> str)
      <*> (lit "b" *> (Show <$> str))
--      <*> (Show <$> str)
      <*> (params' <|> pure [])

    organizations_forums_boards_threads =
      OrganizationsForumsBoardsThreads
      <$> (lit "" *> str)
      <*> (lit "f" *> str)
      <*> (lit "b" *> str)
      <*> (lit "t" *> (Show <$> str))
      <*> (params' <|> pure [])



    resources_leurons_index =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> pure Index)
      <*> (params' <|> pure [])

    resources_leurons_new =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "new" *> pure New)
      <*> (params' <|> pure [])

    resources_leurons_show =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> (Show <$> str1))
      <*> (params' <|> pure [])



    resources_sift =
      ResourcesSiftLeurons
      <$> (lit "" *> lit "resources" *> int <* lit "sift")
      <*> (params' <|> pure [])

    resources_sift_linear_index =
      ResourcesSiftLeuronsLinear
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "sift" *> lit "linear" *> pure Index)
      <*> (params' <|> pure [])

    resources_sift_linear_show_int =
      ResourcesSiftLeuronsLinear
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "sift" *> lit "linear" *> (ShowI <$> int))
      <*> (params' <|> pure [])

    resources_sift_random =
      ResourcesSiftLeuronsRandom
      <$> (lit "" *> lit "resources" *> int <* lit "sift" <* lit "random")
      <*> (params' <|> pure [])



    resources_index =
      Resources
      <$> (lit "" *> lit "resources" *> pure Index)
      <*> (params' <|> pure [])

    resources_new =
      Resources
      <$> (lit "" *> lit "resources" *> lit "new" *> pure New)
      <*> (params' <|> pure [])

    resources_show_int =
      Resources
      <$> (lit "" *> lit "resources" *> (ShowI <$> int))
      <*> (params' <|> pure [])

    resources_edit_int =
      Resources
      <$> (lit "" *> lit "resources" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure [])

    resources_delete_int =
      Resources
      <$> (lit "" *> lit "resources" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure [])



    leurons_index =
      Leurons
      <$> (lit "" *> lit "leurons" *> pure Index)
      <*> (params' <|> pure [])

    leurons_new =
      Leurons
      <$> (lit "" *> lit "leurons" *> lit "new" *> pure New)
      <*> (params' <|> pure [])

    leurons_show_int =
      Leurons
      <$> (lit "" *> lit "leurons" *> (ShowI <$> int))
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
