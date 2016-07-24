{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LN.UI.Router.Route (
  RouteWith (..),
  routeWith,
  routeWith',
  fromRouteWith,
  fromRouteWithHash,
  toRouteWith,
  toRouteWithHash,
  Route (..),
  HasLinkName,
  linkName,
  HasCrumb,
  crumb
) where



import           Control.Applicative        ((<$), (<$>), (<*>), (<|>))
import           Control.DeepSeq            (NFData)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BSC
import           Data.Either                (Either (..))
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (Maybe (..), maybe)
import           Data.Monoid                (mempty, (<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (concat, unpack)
import           Data.Tuple                 (fst)
import           Prelude                    (Eq, Show, fmap, map, pure, show,
                                             ($), (.), (==))
import           Web.Routes

import           Haskell.Api.Helpers.Shared (qp)
import           LN.T
import           LN.UI.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.Router.CRUD
import           LN.UI.Router.Link
import           LN.UI.Router.LinkName      (HasLinkName, linkName)
import           LN.UI.Router.OrderBy
import           LN.UI.Router.Param         (Params, buildParams, emptyParams,
                                             fixParams, fromWebRoutesParams)
import           LN.UI.Router.Util          (slash)
import           LN.UI.Types                (Array, Int, String, Tuple, tuple)



data RouteWith
  = RouteWith Route Params
  deriving (Eq, Show, Generic, NFData)



routeWith :: Route -> [(ParamTag, Param)] -> RouteWith
routeWith route params = RouteWith route (buildParams params)



routeWith' :: Route -> RouteWith
routeWith' route = routeWith route mempty



fromRouteWith :: RouteWith -> Text
fromRouteWith (RouteWith route params) =
  toPathInfoParams route params'
  where
  params' = map (fmap Just . qp) $ Map.elems params



fromRouteWithHash :: RouteWith -> JSString
fromRouteWithHash = textToJSString' . ("#" <>) <$> fromRouteWith



toRouteWith :: ByteString -> RouteWith
toRouteWith url =
  case (fromPathInfoParams url) of
    Left err            -> routeWith' NotFound
    Right (url, params) -> routeWith url $ fromWebRoutesParams params



toRouteWithHash :: ByteString -> RouteWith
toRouteWithHash = toRouteWith . BSC.drop 1



data Route
  = Home
  | About
  | Me
  | Errors
  | Portal
  | Organizations CRUD
  | OrganizationsForums String CRUD
  | OrganizationsForumsBoards String String CRUD
  | OrganizationsForumsBoardsThreads String String String CRUD
  | OrganizationsForumsBoardsThreadsPosts String String String String CRUD
  | OrganizationsTeams String CRUD
  | OrganizationsTeamsMembers String String CRUD
  | OrganizationsMembersOnly String
  | OrganizationsMembership String CRUD
  | Users CRUD
  | UsersProfile String
  | UsersSettings String
  | UsersPMs String
  | UsersThreads String
  | UsersThreadPosts String
  | UsersWorkouts String
  | UsersResources String
  | UsersLeurons String
  | UsersLikes String
  | Resources CRUD
  | ResourcesLeurons Int CRUD
  | ResourcesSiftLeurons Int
  | ResourcesSiftLeuronsLinear Int CRUD
  | ResourcesSiftLeuronsRandom Int
  | Login
  | Logout
  | NotFound
  deriving (Eq, Show, Generic, NFData)



instance HasLinkName Route where
  linkName route = case route of
    Home                            -> "Home"
    About                           -> "About"
    Portal                          -> "Portal"
    Organizations Index             -> "Organizations"
    Organizations (ShowS org_sid)   -> org_sid
    Organizations (EditS org_sid)   -> org_sid
    Organizations (DeleteS org_sid) -> org_sid
    (Users Index)                   -> "Users"
    Users (ShowS user_sid)          -> user_sid
    Users (EditS user_sid)          -> user_sid
    Users (DeleteS user_sid)        -> user_sid
    Login                           -> "Login"
    Logout                          -> "Logout"
    _                               -> "Unknown"



instance HasLinkName RouteWith where
  linkName (RouteWith route params) = linkName route



class HasCrumb a where
  crumb :: a -> [Route]



-- instance HasLink RouteWith where

--   link Home   = tuple "#/"       emptyParams

--   link About  = tuple "#/about"  emptyParams

--   link Me     = tuple "#/me"     emptyParams

--   link Errors = tuple "#/errors" emptyParams

--   link Portal = tuple "#/portal" emptyParams

--   link (Organizations Index params)                  = tuple "#/organizations" (fixParams params)
--   link (Organizations crud@(New) params)             = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
--   link (Organizations crud@(Edit org_name) params)   = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
--   link (Organizations crud@(Delete org_name) params) = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
--   link (Organizations crud@(Show org_name) params)   = tuple ("#" <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsForums org Index params) =
--     tuple ("#/" <> org <> "/f") (fixParams params)
--   link (OrganizationsForums org crud params) =
--     tuple ("#/" <> org <> "/f" <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsForumsBoards org forum crud params) =
--     tuple ("#/" <> org <> "/f/" <> forum <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsForumsBoardsThreads org forum board crud params) =
--     tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) =
--     tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsTeams org crud params) =
--     tuple ("#/" <> org <> "/teams" <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsTeamsMembers org team crud params) =
--     tuple ("#/" <> org <> "/teams/" <> team <> (fst $ link crud)) (fixParams params)

--   link (OrganizationsMembersOnly org) =
--     tuple ("#/" <> org <> "/_members_only") emptyParams

--   link (OrganizationsMembership org crud params) =
--     tuple ("#/" <> org <> "/membership" <> (fst $ link crud)) (fixParams params)

--   link (Users Index params)           = tuple "#/u" (fixParams params)
--   link (Users crud params)            = tuple ("#/u" <> (fst $ link crud)) (fixParams params)
--   link (UsersProfile user params)     = tuple ("#/u/" <> user <> "/profile") (fixParams params)
--   link (UsersSettings user params)    = tuple ("#/u/" <> user <> "/settings") (fixParams params)
--   link (UsersPMs user params)         = tuple ("#/u/" <> user <> "/pms") (fixParams params)
--   link (UsersThreads user params)     = tuple ("#/u/" <> user <> "/threads") (fixParams params)
--   link (UsersThreadPosts user params) = tuple ("#/u/" <> user <> "/thread_posts") (fixParams params)
--   link (UsersWorkouts user params)    = tuple ("#/u/" <> user <> "/workouts") (fixParams params)
--   link (UsersResources user params)   = tuple ("#/u/" <> user <> "/resources") (fixParams params)
--   link (UsersLeurons user params)     = tuple ("#/u/" <> user <> "/leurons") (fixParams params)
--   link (UsersLikes user params)       = tuple ("#/u/" <> user <> "/likes") (fixParams params)

--   link (Resources crud params)                              = tuple ("#/resources" <> (fst $ link crud)) (fixParams params)

--   link (ResourcesLeurons resource_id crud params)           = tuple ("#/resources/" <> show resource_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
--   link (ResourcesSiftLeurons resource_id params) = tuple ("#/resources/" <> show resource_id <> "/sift") (fixParams params)
--   link (ResourcesSiftLeuronsLinear resource_id crud params) = tuple ("#/resources/" <> show resource_id <> "/sift/linear" <> (fst $ link crud)) (fixParams params)
--   link (ResourcesSiftLeuronsRandom resource_id params)      = tuple ("#/resources/" <> show resource_id <> "/sift/random") (fixParams params)

-- --  link (Leurons crud params) = tuple ("#/leurons" <> (fst $ link crud)) (fixParams params)

--   link Login    = tuple "/auth/login" emptyParams
--   link Logout   = tuple "/auth/logout" emptyParams

--   link NotFound = tuple "#/404" emptyParams




instance HasCrumb Route where

  crumb route =
    case route of
       Home   -> []
       About  -> []
       Me     -> []
       Errors -> []
       Portal -> []

       Organizations Index             -> []
       Organizations New               -> [Organizations Index]
       Organizations (ShowS org_sid)   -> [Organizations Index]
       Organizations (EditS org_sid)   -> organizations_repetitive org_sid
       Organizations (DeleteS org_sid) -> organizations_repetitive org_sid

       -- TODO FIXME: Remove eventually, needs to be accurately total
       _ -> [NotFound]

    where
    organizations_repetitive org_sid =
      [ Organizations Index
      , Organizations (ShowS org_sid) ]


--       Organizations Index params ->
--         [
--           tuple (Organizations Index params) "Organizations"
--         ]

--       Organizations New params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations"
--         ]

--       Organizations (Edit org_name) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org_name) emptyParams) org_name
--         ]

--       Organizations (Delete org_name) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org_name) emptyParams) org_name
--         ]

--       Organizations (Show org) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) params) org
--         ]



--       OrganizationsForums org Index params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org
--         ]

--       OrganizationsForums org New params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org
--         ]

--       OrganizationsForums org (Edit forum) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum
--         ]

--       OrganizationsForums org (Delete forum) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum
--         ]

--       OrganizationsForums org (Show forum) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) params) forum
--         ]



--       OrganizationsForumsBoards org forum Index params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum
--         ]

--       OrganizationsForumsBoards org forum New params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum
--         ]

--       OrganizationsForumsBoards org forum (Edit board) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
--         ]

--       OrganizationsForumsBoards org forum (Delete board) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
--         ]

--       OrganizationsForumsBoards org forum (Show board) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) params) board
--         ]



--       OrganizationsForumsBoardsThreads org forum board Index params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
--         ]

--       OrganizationsForumsBoardsThreads org forum board New params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board
--         ]

--       OrganizationsForumsBoardsThreads org forum board (Edit thread) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
--         ]

--       OrganizationsForumsBoardsThreads org forum board (Delete thread) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
--         ]

--       OrganizationsForumsBoardsThreads org forum board (Show thread) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) params) thread
--         ]



--       OrganizationsForumsBoardsThreadsPosts org forum board thread Index params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
--         ]

--       OrganizationsForumsBoardsThreadsPosts org forum board thread New params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread
--         ]

--       OrganizationsForumsBoardsThreadsPosts org forum board thread (EditI post) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread,
--           tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) emptyParams) (show post)
--         ]

--       OrganizationsForumsBoardsThreadsPosts org forum board thread (DeleteI post) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread,
--           tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) emptyParams) (show post)
--         ]

--       OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params ->
--         [
--           tuple (Organizations Index emptyParams) "Organizations",
--           tuple (Organizations (Show org) emptyParams) org,
--           tuple (OrganizationsForums org (Show forum) emptyParams) forum,
--           tuple (OrganizationsForumsBoards org forum (Show board) emptyParams) board,
--           tuple (OrganizationsForumsBoardsThreads org forum board (Show thread) emptyParams) thread,
--           tuple (OrganizationsForumsBoardsThreadsPosts org forum board thread (ShowI post) params) (show post)
--         ]



--       Users Index params ->
--         [
--           tuple (Users Index params) "Users"
--         ]

--       Users (Show user) params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) params) user
--         ]



--       UsersProfile user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersProfile (slash user) params) "Profile"
--         ]

--       UsersSettings user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersSettings (slash user) params) "Settings"
--         ]

--       UsersPMs user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersPMs (slash user) params) "PMs"
--         ]

--       UsersThreads user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersThreads (slash user) params) "Threads"
--         ]

--       UsersThreadPosts user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersThreadPosts (slash user) params) "ThreadPosts"
--         ]

--       UsersWorkouts user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersWorkouts (slash user) params) "Workouts"
--         ]

--       UsersResources user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersResources (slash user) params) "Resources"
--         ]

--       UsersLeurons user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersLeurons (slash user) params) "Leurons"
--         ]

--       UsersLikes user params ->
--         [
--           tuple (Users Index emptyParams) "Users",
--           tuple (Users (Show user) emptyParams) user,
--           tuple (UsersLikes (slash user) params) "Likes"
--         ]



--       Resources Index params ->
--         [tuple (Resources Index params) "Resources"]

--       Resources New params ->
--         [tuple (Resources Index params) "Resources"]

--       Resources (EditI resource_id) params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id params
--         ]

--       Resources (DeleteI resource_id) params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id params
--         ]

--       Resources (ShowI resource_id) params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id params
--         ]



--       ResourcesLeurons resource_id Index params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesLeurons resource_id Index params) "Leurons"
--         ]

--       ResourcesLeurons resource_id New params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesLeurons resource_id Index params) "Leurons"
--         ]

--       ResourcesLeurons resource_id (EditI leuron_id) params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
--           tuple (ResourcesLeurons resource_id (ShowI leuron_id) emptyParams) (show leuron_id)
--         ]

--       ResourcesLeurons resource_id (DeleteI leuron_id) params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
--           tuple (ResourcesLeurons resource_id (ShowI leuron_id) emptyParams) (show leuron_id)
--         ]

--       ResourcesLeurons resource_id (ShowI leuron_id) params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
--           tuple (ResourcesLeurons resource_id (ShowI leuron_id) params) (show leuron_id)
--         ]



--       ResourcesSiftLeurons resource_id params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesSiftLeurons resource_id params) "Sift"
--         ]

--       ResourcesSiftLeuronsRandom resource_id params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesSiftLeurons resource_id params) "Sift"
--         ]

--       ResourcesSiftLeuronsLinear resource_id _ params ->
--         [
--           tuple (Resources Index emptyParams) "Resources",
--           resource_pretty resource_id emptyParams,
--           tuple (ResourcesSiftLeurons resource_id emptyParams) "Sift",
--           tuple (ResourcesSiftLeuronsLinear resource_id Index params) "Linear"
--         ]


--       _ -> [tuple NotFound "Error"]

--     where
--     resource_pretty resource_id params =
--       tuple (Resources (ShowI resource_id) params) ""
-- --        $ maybe (show resource_id) (\pack -> pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse .. displayName_) st.currentResource




-- instance HasOrderBy Routes where
--   orderBy (OrganizationsForumsBoards org forum (Show board) params) = [OrderBy_CreatedAt, OrderBy_ActivityAt]
--   orderBy _                   = []



-- instance Show Routes where
--   show Home   = "Home"
--   show About  = "About"
--   show Me     = "Me"
--   show Errors = "Errors"
--   show Portal = "Portal"
--   show (Organizations crud params) =
--     "Organizations " <> show crud
--   show (OrganizationsForums org crud params) =
--     "OrganizationsForums " <> org <> sp <> show crud
--   show (OrganizationsForumsBoards org forum crud params) =
--     "OrganizationsForumsBoards " <> org <> sp <> forum <> sp <> show crud
--   show (OrganizationsForumsBoardsThreads org forum board crud params) =
--     "OrganizationsForumsBoardsThreads " <> org <> sp <> forum <> sp <> board <> sp <> show crud
--   show (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) =
--     "OrganizationsForumsBoardsThreads " <> org <> sp <> forum <> sp <> board <> sp <> thread <> sp <> show crud
--   show (OrganizationsTeams org crud params) =
--     "OrganizationsTeams " <> org <> sp <> show crud
--   show (OrganizationsTeamsMembers org team crud params) =
--     "OrganizationsTeamsMembers " <> org <> sp <> team <> sp <> show crud
--   show (OrganizationsMembersOnly org) =
--     "OrganizationsMembersOnly " <> org
--   show (OrganizationsMembership org crud params) =
--     "OrganizationsMembership " <> org <> sp <> show crud
--   show (Users crud params)            = "Users " <> show crud
--   show (UsersProfile user params)     = "UsersProfile " <> user
--   show (UsersSettings user params)    = "UsersSettings " <> user
--   show (UsersPMs user params)         = "UsersPMs " <> user
--   show (UsersThreads user params)     = "UsersThreads " <> user
--   show (UsersThreadPosts user params) = "UsersThreadPosts " <> user
--   show (UsersWorkouts user params)    = "UsersWorkouts " <> user
--   show (UsersResources user params)   = "UsersResources " <> user
--   show (UsersLeurons user params)     = "UsersLeurons " <> user
--   show (UsersLikes user params)       = "UsersLikes " <> user
--   show (Resources crud params)        = "Resources " <> show crud
--   show (ResourcesLeurons resource_id crud params)           = "ResourcesLeurons " <> show resource_id <> sp <> show crud
--   show (ResourcesSiftLeurons resource_id params)            = "ResourcesSiftLeurons " <> show resource_id
--   show (ResourcesSiftLeuronsLinear resource_id crud params) = "ResourcesSiftLeuronsLinear " <> show resource_id <> sp <> show crud
--   show (ResourcesSiftLeuronsRandom resource_id params)      = "ResourcesSiftLeuronsRandom " <> show resource_id
--   show Login    = "Login"
--   show Logout   = "Logout"
--   show NotFound = "NotFound"
--   show _ = "make sure Show covers all Routes"



-- sp :: String
-- sp = " "


prep :: Text -> [Text]
prep route = pure $ "#/" <> route



preps :: Text -> Text
preps = Text.concat . prep



instance PathInfo Route where

  toPathSegments route = case route of
    Home                     -> pure ""
    About                    -> pure "about"
    Me                       -> pure "me"
    Errors                   -> pure "errors"
    Portal                   -> pure "portal"
    Organizations Index      -> pure "organizations"
    Organizations (ShowS s)  -> pure s
    Organizations crud       -> (pure $ "organizations") <> toPathSegments crud
    Users Index              -> pure "users"
    Users crud               -> (pure $ "users") <> toPathSegments crud
    _                        -> pure ""

  fromPathSegments =
        About         <$ segment "about"
    <|> Me            <$ segment "me"
    <|> Errors        <$ segment "errors"
    <|> Portal        <$ segment "portal"
    <|> Organizations <$ segment "organizations" <*> fromPathSegments
    <|> Users         <$ segment "users" <*> fromPathSegments
    <|> pure Home
    -- TODO FIXME: Can't do Home <$ segment "" because it fails to pattern match. Though, pure Index works because it's terminal.


  -- link Errors = tuple "#/errors" emptyParams

  -- link Portal = tuple "#/portal" emptyParams

  -- link (Organizations Index params)                  = tuple "#/organizations" (fixParams params)
  -- link (Organizations crud@(New) params)             = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  -- link (Organizations crud@(Edit org_name) params)   = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  -- link (Organizations crud@(Delete org_name) params) = tuple ("#/organizations" <> (fst $ link crud)) (fixParams params)
  -- link (Organizations crud@(Show org_name) params)   = tuple ("#" <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsForums org Index params) =
  --   tuple ("#/" <> org <> "/f") (fixParams params)
  -- link (OrganizationsForums org crud params) =
  --   tuple ("#/" <> org <> "/f" <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsForumsBoards org forum crud params) =
  --   tuple ("#/" <> org <> "/f/" <> forum <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsForumsBoardsThreads org forum board crud params) =
  --   tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsForumsBoardsThreadsPosts org forum board thread crud params) =
  --   tuple ("#/" <> org <> "/f/" <> forum <> "/" <> board <> "/" <> thread <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsTeams org crud params) =
  --   tuple ("#/" <> org <> "/teams" <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsTeamsMembers org team crud params) =
  --   tuple ("#/" <> org <> "/teams/" <> team <> (fst $ link crud)) (fixParams params)

  -- link (OrganizationsMembersOnly org) =
  --   tuple ("#/" <> org <> "/_members_only") emptyParams

  -- link (OrganizationsMembership org crud params) =
  --   tuple ("#/" <> org <> "/membership" <> (fst $ link crud)) (fixParams params)

  -- link (Users Index params)           = tuple "#/u" (fixParams params)
  -- link (Users crud params)            = tuple ("#/u" <> (fst $ link crud)) (fixParams params)
  -- link (UsersProfile user params)     = tuple ("#/u/" <> user <> "/profile") (fixParams params)
  -- link (UsersSettings user params)    = tuple ("#/u/" <> user <> "/settings") (fixParams params)
  -- link (UsersPMs user params)         = tuple ("#/u/" <> user <> "/pms") (fixParams params)
  -- link (UsersThreads user params)     = tuple ("#/u/" <> user <> "/threads") (fixParams params)
  -- link (UsersThreadPosts user params) = tuple ("#/u/" <> user <> "/thread_posts") (fixParams params)
  -- link (UsersWorkouts user params)    = tuple ("#/u/" <> user <> "/workouts") (fixParams params)
  -- link (UsersResources user params)   = tuple ("#/u/" <> user <> "/resources") (fixParams params)
  -- link (UsersLeurons user params)     = tuple ("#/u/" <> user <> "/leurons") (fixParams params)
  -- link (UsersLikes user params)       = tuple ("#/u/" <> user <> "/likes") (fixParams params)

  -- link (Resources crud params)                              = tuple ("#/resources" <> (fst $ link crud)) (fixParams params)

  -- link (ResourcesLeurons resource_id crud params)           = tuple ("#/resources/" <> show resource_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
  -- link (ResourcesSiftLeurons resource_id params) = tuple ("#/resources/" <> show resource_id <> "/sift") (fixParams params)
  -- link (ResourcesSiftLeuronsLinear resource_id crud params) = tuple ("#/resources/" <> show resource_id <> "/sift/linear" <> (fst $ link crud)) (fixParams params)
  -- link (ResourcesSiftLeuronsRandom resource_id params)      = tuple ("#/resources/" <> show resource_id <> "/sift/random") (fixParams params)

-- --  link (Leurons crud params) = tuple ("#/leurons" <> (fst $ link crud)) (fixParams params)

  -- link Login    = tuple "/auth/login" emptyParams
  -- link Logout   = tuple "/auth/logout" emptyParams

  -- link NotFound = tuple "#/404" emptyParams
