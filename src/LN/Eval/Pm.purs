module LN.Eval.Pm (
  eval_Pm
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, catMaybes, (:))
import Data.Ebyam                      (ebyam)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Data.String                     (toLower)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, map, ($), (<>), (<<<))

import LN.Api                          (rd)
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Pm                     (InputPm(..), Pm_Act(..), Pm_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.State.Loading                ()
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.PageInfo               (runPageInfo)
import LN.T.Internal.Convert           ()
import LN.T



eval_Pm :: EvalEff
eval_Pm eval (CompPm sub next) = do

  org_pack   <- gets _.currentOrganization
  forum_pack <- gets _.currentForum
  board_pack <- gets _.currentBoard
  let
    org_name   = maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) org_pack
    forum_name = maybe "unknown" (\org -> org ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. name_) forum_pack
    board_name = maybe "unknown" (\org -> org ^. _BoardPackResponse .. board_ ^. _BoardResponse .. name_) board_pack

  case sub of

    InputPm_Act q -> do
      case q of
        _ -> pure next



    InputPm_Mod q -> do
      case q of
        SetSubject s -> mod $ set (\req -> _PmRequest .. subject_ .~ s $ req)
        SetBody s    -> mod $ set (\req -> _PmRequest .. body_ .~ s $ req)
        Send user_id -> mod_send user_id
        EditP pm_id  -> mod_edit pm_id


    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentPmRequest = maybe Nothing new st.currentPmRequest }) $> next



  mod_send user_id = do
    pure next

  mod_edit pm_id = do
    pure next
