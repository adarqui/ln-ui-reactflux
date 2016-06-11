module LN.Input.Types (
  Input (..),
  -- helpers
  cOrganization,
  cOrganizationMod,
  cOrganizationAct,
  cTeam,
  cTeamMod,
  cForum,
  cForumMod,
  cBoard,
  cBoardMod,
  cThread,
  cThreadMod,
  cThreadPost,
  cThreadPostMod,
  cResource,
  cResourceMod,
  cLeuron,
  cLeuronMod
) where


import Data.Foreign            (ForeignError)
import Purescript.Api.Helpers  (ApiError)

import LN.Input.Board          (InputBoard(..), Board_Mod)
import LN.Input.Forum          (InputForum(..), Forum_Mod)
import LN.Input.Leuron         (InputLeuron(..), Leuron_Mod)
import LN.Input.Like           (InputLike)
import LN.Input.Star           (InputStar)
import LN.Input.OrderBy        (InputOrderBy)
import LN.Input.Organization   (InputOrganization(..), Organization_Act, Organization_Mod)
import LN.Input.Profile        (InputProfile)
import LN.Input.Resource       (InputResource(..), Resource_Mod)
import LN.Input.Team           (InputTeam(..), Team_Mod)
import LN.Input.Thread         (InputThread(..), Thread_Mod)
import LN.Input.ThreadPost     (InputThreadPost(..), ThreadPost_Mod)
import LN.Router.Class.Routes  (Routes)
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
  | GetOrganizationId Int a
  | GetOrganizationForum String String a
  | GetOrganizationForumBoard String String String a
  | GetOrganizationForumBoardThread String String String String a
  | GetOrganizationForumBoardThreadPost String String String String Int a

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

  | CompOrderBy        InputOrderBy      a
  | CompOrganization   InputOrganization a
  | CompTeam           InputTeam         a
  | CompForum          InputForum        a
  | CompBoard          InputBoard        a
  | CompThread         InputThread       a
  | CompThreadPost     InputThreadPost   a
  | CompProfile        InputProfile      a
  | CompLike           InputLike         a
  | CompStar           InputStar         a
  | CompResource       InputResource     a
  | CompLeuron         InputLeuron       a

  | Nop a



-- | Helpers for "components" and "subcomponents"
--

cOrganization :: forall a. InputOrganization -> a -> Input a
cOrganization sub next = CompOrganization sub next

cOrganizationMod :: forall a. Organization_Mod -> a -> Input a
cOrganizationMod mod next = CompOrganization (InputOrganization_Mod mod) next

cOrganizationAct :: forall a. Organization_Act -> a -> Input a
cOrganizationAct act next = CompOrganization (InputOrganization_Act act) next



cTeam :: forall a. InputTeam -> a -> Input a
cTeam il next = CompTeam il next

cTeamMod :: forall a. Team_Mod -> a -> Input a
cTeamMod lm next = CompTeam (InputTeam_Mod lm) next



cForum :: forall a. InputForum -> a -> Input a
cForum il next = CompForum il next

cForumMod :: forall a. Forum_Mod -> a -> Input a
cForumMod lm next = CompForum (InputForum_Mod lm) next



cBoard :: forall a. InputBoard -> a -> Input a
cBoard il next = CompBoard il next

cBoardMod :: forall a. Board_Mod -> a -> Input a
cBoardMod lm next = CompBoard (InputBoard_Mod lm) next



cThread :: forall a. InputThread -> a -> Input a
cThread il next = CompThread il next

cThreadMod :: forall a. Thread_Mod -> a -> Input a
cThreadMod lm next = CompThread (InputThread_Mod lm) next



cThreadPost :: forall a. InputThreadPost -> a -> Input a
cThreadPost il next = CompThreadPost il next

cThreadPostMod :: forall a. ThreadPost_Mod -> a -> Input a
cThreadPostMod lm next = CompThreadPost (InputThreadPost_Mod lm) next



cResource :: forall a. InputResource -> a -> Input a
cResource ir next = CompResource ir next

cResourceMod :: forall a. Resource_Mod -> a -> Input a
cResourceMod rm next = CompResource (InputResource_Mod rm) next



cLeuron :: forall a. InputLeuron -> a -> Input a
cLeuron il next = CompLeuron il next

cLeuronMod :: forall a. Leuron_Mod -> a -> Input a
cLeuronMod lm next = CompLeuron (InputLeuron_Mod lm) next
