module LN.Eval.Forums (
  eval_GetForums,
  eval_GetForumsForOrg
) where



import Control.Monad.Aff.Console       (log)
import Data.Array                      (zip)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Halogen                         (modify, liftAff')
import Prelude                         (bind, pure, map, show, ($), (<>))

import LN.Api                          (rd, getForumPacks_ByOrganizationName')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T                            (ForumPackResponses(..), ForumPackResponse(..))



eval_GetForums :: EvalEff
eval_GetForums eval (GetForums next) = do

  pure next

{- TODO FIXME: do we use this?
  eforums <- rd $ getForums'
  case eforums of
    Left err -> pure next
    Right (ForumResponses forums) -> do
      modify (_{ forums = forums.forumResponses })
      pure next
      -}



eval_GetForumsForOrg :: EvalEff
eval_GetForumsForOrg eval (GetForumsForOrg org_name next) = do

  eforum_packs <- rd $ getForumPacks_ByOrganizationName' org_name
  case eforum_packs of
    Left err -> liftAff' $ log ("getForumPacks_ByOrgName: Error: " <> show err) $> next
    Right (ForumPackResponses forum_packs) -> do

      let
        forums     = forum_packs.forumPackResponses
        forums_map = M.fromFoldable $ zip (map (\(ForumPackResponse pack) -> pack.forumId) forums) forums

      modify (_{ forums = forums_map })
      pure next
