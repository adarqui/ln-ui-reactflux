module LN.Eval.Forums (
  eval_GetForums,
  eval_GetForumsForOrg
) where



import Data.Either                     (Either(..))
import Data.Map                        as M
import Halogen                         (modify)
import Prelude                         (bind, pure, ($))

import LN.Api                          (rd, getForumPacks_ByOrganizationName')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.Helpers.Map                  (idmapFrom)
import LN.T                            (ForumPackResponses(..), ForumPackResponse(..))



eval_GetForums :: EvalEff
eval_GetForums eval (GetForums next) = pure next



eval_GetForumsForOrg :: EvalEff
eval_GetForumsForOrg eval (GetForumsForOrg org_name next) = do

  modify (_{ forums = (M.empty :: M.Map Int ForumPackResponse) })

  e_forum_packs <- rd $ getForumPacks_ByOrganizationName' org_name
  case e_forum_packs of
    Left err                               -> eval (AddErrorApi "eval_GetForumsForOrg::getForumPacks_ByOrgName'" err next)
    Right (ForumPackResponses forum_packs) -> do

      let
        forums_map = idmapFrom (\(ForumPackResponse pack) -> pack.forumId) forum_packs.forumPackResponses

      modify (_{ forums = forums_map })
      pure next
