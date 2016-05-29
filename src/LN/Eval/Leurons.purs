module LN.Eval.Leurons (
  eval_GetLeurons,
  eval_GetLeuronId,
  eval_GetLeuronRandom,
  eval_Leuron
) where



import Data.Array                    (head, deleteAt, modifyAt, nub, (:))
import Data.Either                   (Either(..))
import Data.Functor                  (($>))
import Data.Int                      (fromString)
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Halogen                       (gets, modify)
import Optic.Core                    ((^.), (..), (.~))
import Prelude                       (class Eq, id, const, bind, pure, map, ($), (<>), (<<<))

import LN.Api                        ( rd, getLeuronsCount', getLeuronPack', getLeuronPacks
                                     , postLeuron_ByResourceId', putLeuron')
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Leuron               (InputLeuron(..), Leuron_Mod(..))
import LN.Input.Types                (Input(..))
import LN.State.Leuron               (leuronRequestStateFromLeuronData)
import LN.State.Loading              (setLoading, clearLoading, l_currentLeuron, l_leurons)
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( LeuronPackResponses(..), LeuronPackResponse(..)
                                     , LeuronResponse(..)
                                     , LeuronRequest(..)
                                     , _LeuronRequest
                                     , title_, description_, section_, examples_, page_
                                     , LeuronData(..)
                                     , Param(..), SortOrderBy(..))



eval_GetLeurons :: EvalEff
eval_GetLeurons eval (GetLeurons next) = do

  modify (_{ leurons = (M.empty :: M.Map Int LeuronPackResponse) })

  page_info <- gets _.leuronsPageInfo

  e_count <- rd $ getLeuronsCount'
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetLeurons::getLeuronsCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ leuronsPageInfo = new_page_info.pageInfo })
      modify (\st->st{loading = setLoading l_leurons st.loading})

      e_leuron_packs <- rd $ getLeuronPacks new_page_info.params

      modify (\st->st{loading = clearLoading l_leurons st.loading})

      case e_leuron_packs of
           Left err                                 -> eval (AddErrorApi "eval_GetLeurons::getLeuronPacks" err next)
           Right (LeuronPackResponses leuron_packs) -> do

             let
              users       = map (\(LeuronPackResponse pack) -> pack.user) leuron_packs.leuronPackResponses
              leurons_map = idmapFrom (\(LeuronPackResponse p) -> p.leuronId) leuron_packs.leuronPackResponses


             eval (GetUsers_MergeMap_ByUser users next)


             modify (_{ leurons = leurons_map })
             pure next



eval_GetLeuronId :: EvalEff
eval_GetLeuronId eval (GetLeuronId leuron_id next) = do

  modify (_{ currentLeuron = Nothing })
  modify (\st->st{loading = setLoading l_currentLeuron st.loading})

  e_pack <- rd $ getLeuronPack' leuron_id

  modify (\st->st{loading = clearLoading l_currentLeuron st.loading})

  case e_pack of
    Left err   -> eval (AddErrorApi "eval_GetLeuronId::getLeuronPack'" err next)
    Right pack -> do
      modify (_{ currentLeuron = Just pack })
      pure next



eval_GetLeuronRandom :: EvalEff
eval_GetLeuronRandom eval (GetLeuronRandom next) = do

  modify (_{ currentLeuron = Nothing })
  modify (\st->st{loading = setLoading l_currentLeuron st.loading})

  e_packs <- rd $ getLeuronPacks [Limit 1, SortOrder SortOrderBy_Rnd]

  modify (\st->st{loading = clearLoading l_currentLeuron st.loading})

  case e_packs of
    Left err                          -> eval (AddErrorApi "eval_GetLeuronRandom::getLeuronPacks" err next)
    Right (LeuronPackResponses packs) -> do
      case head packs.leuronPackResponses of
        Nothing   -> pure next
        Just pack -> modify (_{ currentLeuron = Just pack }) $> next




eval_Leuron :: EvalEff
eval_Leuron eval (CompLeuron sub next) = do
  case sub of
    InputLeuron_Mod q -> do
      case q of

        SetTitle title      -> mod $ set (\req -> _LeuronRequest .. title_ .~ Just title $ req)
        EditTitle title     -> pure next
        RemoveTitle         -> pure next

        SetDescription desc  -> mod $ set (\req -> _LeuronRequest .. description_ .~ Just desc $ req)
        EditDescription desc -> pure next
        RemoveDescription    -> pure next

        SetSection section  -> mod $ set (\req -> _LeuronRequest .. section_ .~ Just section $ req)
        EditSection section -> pure next
        RemoveSection       -> pure next

--        SetPage page        -> mod $ set (\req -> _LeuronRequest .. page_ .~ Just page $ req)
--        EditPage page       -> pure next
--        RemovePage          -> pure next

        SetExample ex       -> modSt (_{exampleItem = ex})
        AddExample ex       -> do
          mod (\(LeuronRequest req)->Just $ LeuronRequest req{examples = Just $ maybe [ex] (\examples->examples <> [ex]) req.examples})
          modSt (_{exampleItem = ""})
          pure next
        EditExample idx new -> pure next
        DeleteExample idx   -> pure next
        ClearExamples       -> mod $ set (\req -> _LeuronRequest .. examples_ .~ Nothing $ req)

        SetData d           -> do
          mod $ set (\(LeuronRequest req) -> LeuronRequest req{ dataP = d })
          modSt (leuronRequestStateFromLeuronData d) $> next

        SetType ty          -> modSt (_{ty = ty}) $> next

        -- We need this because there are ancillary items associated with LeuronData types, on the front end
        -- for example, an item that being edited prior to its inclusing in a list
        -- ie, factList_listItem is eventually added to factList.list
        SetSt lst           -> modSt (const lst) $> next

        Save resource_id   -> do

          m_req <- gets _.currentLeuronRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Leuron(Save)" "Leuron request doesn't exist" next)
               Just req -> do

                 e_leuron <- rd $ postLeuron_ByResourceId' resource_id req
                 case e_leuron of
                      Left err                      -> eval (AddErrorApi "eval_Leuron(Save)::postLeuron'" err next)
                      Right (LeuronResponse leuron) -> modSt (\lst-> lst{ids = leuron.id : lst.ids}) $> next

        EditP leuron_id    -> do

          m_req <- gets _.currentLeuronRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Leuron(Edit)" "Leuron request doesn't exist" next)
               Just req -> do

                 e_leuron <- rd $ putLeuron' leuron_id req
                 case e_leuron of
                      Left err                          -> eval (AddErrorApi "eval_Leuron(Edit)::putLeuron'" err next)
                      Right (LeuronResponse leuron) -> pure next
--                        eval (Goto (Leurons (ShowI leuron.id) []) next)

    InputLeuron_Nop         -> pure next

  where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req                = Just (v req)
  mod new                  = modify (\st->st{ currentLeuronRequest = maybe Nothing new st.currentLeuronRequest }) $> next
  modSt new                = modify (\st->st{ currentLeuronRequestSt = maybe Nothing (Just <<< new) st.currentLeuronRequestSt }) $> next
