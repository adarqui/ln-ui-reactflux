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
  cForumAct,
  cBoard,
  cBoardMod,
  cBoardAct,
  cThread,
  cThreadMod,
  cThreadAct,
  cThreadPost,
  cThreadPostMod,
  cThreadPostAct,
  cResource,
  cResourceMod,
  cLeuron,
  cLeuronMod
) where


import Data.Foreign            (ForeignError)
import Purescript.Api.Helpers  (ApiError)

import LN.Input.Board          (InputBoard(..), Board_Act, Board_Mod)
import LN.Input.Forum          (InputForum(..), Forum_Act, Forum_Mod)
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

  | GetTeams a

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
cTeam sub next = CompTeam sub next

cTeamMod :: forall a. Team_Mod -> a -> Input a
cTeamMod mod next = CompTeam (InputTeam_Mod mod) next



cForum :: forall a. InputForum -> a -> Input a
cForum sub next = CompForum sub next

cForumMod :: forall a. Forum_Mod -> a -> Input a
cForumMod mod next = CompForum (InputForum_Mod mod) next

cForumAct :: forall a. Forum_Act -> a -> Input a
cForumAct act next = CompForum (InputForum_Act act) next



cBoard :: forall a. InputBoard -> a -> Input a
cBoard sub next = CompBoard sub next

cBoardMod :: forall a. Board_Mod -> a -> Input a
cBoardMod mod next = CompBoard (InputBoard_Mod mod) next

cBoardAct :: forall a. Board_Act -> a -> Input a
cBoardAct act next = CompBoard (InputBoard_Act act) next



cThread :: forall a. InputThread -> a -> Input a
cThread sub next = CompThread sub next

cThreadMod :: forall a. Thread_Mod -> a -> Input a
cThreadMod mod next = CompThread (InputThread_Mod mod) next

cThreadAct :: forall a. Thread_Act -> a -> Input a
cThreadAct act next = CompThread (InputThread_Act act) next



cThreadPost :: forall a. InputThreadPost -> a -> Input a
cThreadPost sub next = CompThreadPost sub next

cThreadPostMod :: forall a. ThreadPost_Mod -> a -> Input a
cThreadPostMod mod next = CompThreadPost (InputThreadPost_Mod mod) next

cThreadPostAct :: forall a. ThreadPost_Act -> a -> Input a
cThreadPostAct act next = CompThreadPost (InputThreadPost_Act act) next



cResource :: forall a. InputResource -> a -> Input a
cResource ir next = CompResource ir next

cResourceMod :: forall a. Resource_Mod -> a -> Input a
cResourceMod rm next = CompResource (InputResource_Mod rm) next



cLeuron :: forall a. InputLeuron -> a -> Input a
cLeuron sub next = CompLeuron sub next

cLeuronMod :: forall a. Leuron_Mod -> a -> Input a
cLeuronMod mod next = CompLeuron (InputLeuron_Mod mod) next
