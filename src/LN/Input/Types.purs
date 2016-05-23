module LN.Input.Types (
  Input (..)
) where


import Data.Foreign            (ForeignError)
import Purescript.Api.Helpers  (ApiError)

import LN.Input.CreateThread   (InputCreateThread)
import LN.Input.Like           (InputLike)
import LN.Input.Star           (InputStar)
import LN.Input.OrderBy        (InputOrderBy)
import LN.Input.Profile        (InputProfile)
import LN.Input.Resource       (InputResource)
import LN.Input.ThreadPost     (InputThreadPost)
import LN.Router.Types         (Routes)
import LN.T



data Input a
  = Goto Routes a

  | AddError String String a
  | AddErrorF String ForeignError a
  | AddErrorApi String ApiError a
  | DelError Int a
  | ClearErrors a

  | GetUser String a
  | GetMe a
  | GetUsers a
  | GetUsers_MergeMap_ByUser (Array UserSanitizedResponse) a
  | GetUsers_MergeMap_ByUserId (Array Int) a

  | GetOrganizations a
  | GetOrganization String a
  | GetOrganizationForum String String a
  | GetOrganizationForumBoard String String String a
  | GetOrganizationForumBoardThread String String String String a

  | GetTeams a

  | GetForums a
  | GetForumsForOrg String a

  | GetBoards a
  | GetBoardsForForum Int a

  | GetThreads a
  | GetThreadsForBoard Int a

  | GetThreadPosts a
  | GetThreadPostsForThread Int a

  | GetThreadPost String a
  | GetThreadPostLikes a

  | GetPMs a

  | GetResources a
  | GetResourceId Int a
  | GetResourceSid String a

  | GetResourcesLeurons Int a
  | GetResourceLeuronRandom Int a

  | GetResourcesSiftLeurons Int a

  | GetLeurons a
  | GetLeuronId Int a
  | GetLeuronSid String a
  | GetLeuronRandom a

  | ConnectSocket a

  | CompThreadPost InputThreadPost a

  | CompCreateThread InputCreateThread a

  | CompOrderBy InputOrderBy a

  | CompProfile InputProfile a

  | CompLike InputLike a

  | CompStar InputStar a

  | CompResource InputResource a

  | Nop a
