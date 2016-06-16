module LN.State.ArrayString (
  ArrayStringState(..),
  defaultArrayStringState
) where



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
