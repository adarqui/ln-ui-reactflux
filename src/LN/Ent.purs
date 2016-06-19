module LN.Ent (
  createByParamFromEnt
) where



import Data.Maybe          (Maybe(..))
import Prelude             (($))

import LN.Input.ThreadPost as ThreadPost
import LN.Input.Types      (Input, cThreadPostAct)
import LN.T                (Param(..), Ent(..))



createByParamFromEnt :: Ent -> Int -> Maybe Param
createByParamFromEnt ent ent_id =
  case ent of
       Ent_ThreadPost -> Just $ ByThreadPostId ent_id
       _              -> Nothing



createResyncFromEnt :: forall a. Ent -> Int -> a -> Input a
createResyncFromEnt ent ent_id next =
  case ent of
       Ent_ThreadPost -> (cThreadPostAct (ThreadPost.ResyncById ent_id) next)
