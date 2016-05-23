module LN.Input.Star (
  InputStar (..)
) where



import Data.Maybe (Maybe)

import LN.T       (Ent, StarResponse, StarResponse)


data InputStar
  = InputStar Ent Int (Maybe StarResponse)
