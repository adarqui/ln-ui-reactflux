module LN.State.ArrayString (
  ArrayStringState(..),
  defaultArrayStringState,
  getArrayStringCurrent,
  getArrayStringEnt
) where



import Data.Maybe           (Maybe(..))
import Data.Map             as M

import LN.Input.ArrayString (ArrayStringEnt)



type ArrayStringState = {
  ents     :: M.Map ArrayStringEnt (Array String),
  currents :: M.Map ArrayStringEnt String
}



defaultArrayStringState :: ArrayStringState
defaultArrayStringState = {
  ents:     M.empty,
  currents: M.empty
}



getArrayStringCurrent :: ArrayStringEnt -> M.Map ArrayStringEnt String -> String
getArrayStringCurrent ent m =
  case M.lookup ent m of
       Nothing -> ""
       Just s  -> s



getArrayStringEnt :: ArrayStringEnt -> M.Map ArrayStringEnt (Array String) -> Array String
getArrayStringEnt ent m =
  case M.lookup ent m of
       Nothing  -> []
       Just arr -> arr
