module LN.Eval.Memberships (
  eval_Membership
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, (:))
import Data.Ebyam                      (ebyam)
import Data.Either                     (Either(..))
import Data.Foldable                   (elem)
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Data.String                     (toLower)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, const, ($), (<>), (<$>), (<<<), (==))

import LN.Api                          (rd, postTeamMember_ByOrganizationId')
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Membership             (InputMembership(..), Membership_Act(..), Membership_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (l_currentOrganization)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.T.Internal.Convert           ()
import LN.T                            ( SystemTeam(..), Membership(..)
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_, organizationId_, name_
                                       , TeamPackResponse(..), _TeamPackResponse
                                       , _TeamResponse, team_, teams_
                                       , mkTeamMemberRequest)



eval_Membership :: EvalEff
eval_Membership eval (CompMembership sub next) = do

  m_me <- gets _.me

  case sub of

    InputMembership_Act q -> do
      case q of
        Join_ByCurrentOrganization  -> act_join
        Leave_ByCurrentOrganization -> act_leave


    InputMembership_Mod q -> pure next

    _   -> pure next

 where

  act_join = do
    m_org_pack <- gets _.currentOrganization
    case m_org_pack of
      Nothing       -> eval (AddError "eval_Team(Act/Join)" "Organization doesn't exist" next)
      Just org_pack -> do
        let org = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
        if Team_Members `elem` (org_pack ^. _OrganizationPackResponse .. teams_)
           then
             -- Already a member
             eval (Goto (Organizations (Show org.name) emptyParams) next)
           else do
             let team_member_request = mkTeamMemberRequest 0
             e_resp <- rd $ postTeamMember_ByOrganizationId' (org_pack ^. _OrganizationPackResponse .. organizationId_) team_member_request
             case e_resp of
              Left err -> eval (AddErrorApi "eval_Team(Act/Join)::postTeamMember_ByOrganizationId'" err next)
              Right resp -> eval (Goto (Organizations (Show org.name) emptyParams) next)

    pure next

  act_leave = do
--    delete team members by ... team id
    pure next
