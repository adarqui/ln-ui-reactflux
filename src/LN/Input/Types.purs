module LN.Input.Types (
  Input (..),
  -- helpers
  cResource,
  cResourceMod,
  cLeuron,
  cLeuronMod,
  cLeuronNop
) where


import Data.Foreign            (ForeignError)
import Purescript.Api.Helpers  (ApiError)
import Prelude                 (Unit, unit, (<<<))

import LN.Input.CreateThread   (InputCreateThread)
import LN.Input.Leuron         (InputLeuron(..), Leuron_Mod)
import LN.Input.Like           (InputLike)
import LN.Input.Star           (InputStar)
import LN.Input.OrderBy        (InputOrderBy)
import LN.Input.Profile        (InputProfile)
import LN.Input.Resource       (InputResource(..), Resource_Mod)
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

  | GetResourcesLeurons Int a
  | GetResourceLeuronLinear Int Int a
  | GetResourceLeuronRandom Int a

  | GetResourcesSiftLeurons Int a

  | GetLeurons a
  | GetLeuronId Int a
  | GetLeuronRandom a

  | ConnectSocket a

  | CompThreadPost InputThreadPost a

  | CompCreateThread InputCreateThread a

  | CompOrderBy InputOrderBy a

  | CompProfile InputProfile a

  | CompLike InputLike a

  | CompStar InputStar a

  | CompResource InputResource a

  | CompLeuron InputLeuron a

  | Nop a



-- | Helpers for "components" and "subcomponents"
--

cResource :: forall a. InputResource -> a -> Input a
cResource ir next = CompResource ir next

cResourceMod :: forall a. Resource_Mod -> a -> Input a
cResourceMod rm next = CompResource (InputResource_Mod rm) next



cLeuron :: forall a. InputLeuron -> a -> Input a
cLeuron il next = CompLeuron il next

cLeuronMod :: forall a. Leuron_Mod -> a -> Input a
cLeuronMod lm next = CompLeuron (InputLeuron_Mod lm) next

cLeuronNop :: forall a. a -> Input a
cLeuronNop next = CompLeuron InputLeuron_Nop next
