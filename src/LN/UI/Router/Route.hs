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
import           LN.UI.Router.CRUD          (CRUD (..))
import           LN.UI.Router.Crumb         (HasCrumb, crumb)
import           LN.UI.Router.LinkName      (HasLinkName, linkName)
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
    Organizations New               -> "New"
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
    <|> Organizations <$> (ShowS <$> anySegment)
    <|> pure Home
    -- TODO FIXME: Can't do Home <$ segment "" because it fails to pattern match. Though, pure Index works because it's terminal.
