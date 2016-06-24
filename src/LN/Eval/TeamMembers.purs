module LN.Eval.TeamMembers (
  eval_TeamMember
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, (:))
import Data.Ebyam                      (ebyam)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Data.String                     (toLower)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, const, ($), (<>), (<$>), (<<<))

import LN.Api                          (rd, putTeamMember', getTeamMemberPacks_ByTeamId')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.TeamMember             (InputTeamMember(..), TeamMember_Act(..), TeamMember_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (l_currentTeamMember, l_teams)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.State.TeamMember             (TeamMemberRequestState, defaultTeamMemberRequestState)
import LN.T.Internal.Convert           ()
import LN.T                            ( OrganizationPackResponses(..), OrganizationPackResponse(..)
                                       , OrganizationResponse(..)
                                       , _TeamPackResponse, teamId_
                                       , _TeamMemberPackResponse, TeamMemberPackResponse(..), TeamMemberPackResponses(..)
                                       , _TeamMemberResponse, TeamMemberResponse(..)
                                       , _TeamMemberRequest, TeamMemberRequest(..)
                                       , membership_, visibility_)



eval_TeamMember :: EvalEff
eval_TeamMember eval (CompTeamMember sub next) = do

  m_me <- gets _.me

  case sub of

    InputTeamMember_Act q -> do
      case q of
        Gets_ByCurrentTeam   -> act_gets_by_current_team
        GetId team_member_id -> pure next



    InputTeamMember_Mod q -> do
      case q of
        _ -> pure next

    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentTeamMemberRequest = maybe Nothing new st.currentTeamMemberRequest }) $> next



  act_gets_by_current_team = do
    m_team <- gets _.currentTeam
    case m_team of
      Nothing        -> eval (AddError "eval_TeamMember(Act/Gets)" "Team doesn't exist" next)
      Just team_pack -> do
        let team_id = team_pack ^. _TeamPackResponse .. teamId_
        e_team_members <- rd $ getTeamMemberPacks_ByTeamId' team_id
        case e_team_members of
          Left err -> eval (AddErrorApi "eval_TeamMember(Act/Gets)::getTeamMemberPacks_ByTeamId'" err next)
          Right (TeamMemberPackResponses team_member_packs) -> do
            let team_members_map = idmapFrom (\(TeamMemberPackResponse pack) -> pack.teamMemberId) team_member_packs.teamMemberPackResponses
            modify (_{ teamMembers = team_members_map })
            pure next
