module LN.Eval.Memberships (
  eval_Membership
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

import LN.Api                          (rd)
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Membership             (InputMembership(..), Membership_Act(..), Membership_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (l_currentOrganization)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.T.Internal.Convert           ()
import LN.T                            ()



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
--    post team members by ... team id
--    need teams and team members in state
    pure next

  act_leave = do
--    delete team members by ... team id
    pure next
