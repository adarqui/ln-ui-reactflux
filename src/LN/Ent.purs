module LN.Ent (
  createByParamFromEnt
) where



import Data.Maybe (Maybe(..))
import Prelude    (($))

import LN.T       (Param(..), Ent(..))



createByParamFromEnt :: Ent -> Int -> Maybe Param
createByParamFromEnt ent ent_id =
  case ent of
       Ent_ThreadPost -> Just $ ByThreadPostId ent_id
       _              -> Nothing
