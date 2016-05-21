module LN.Internal.Leuron (
  LeuronSift (..)
) where



import Data.Generic                (class Generic, gEq, gShow)
import Prelude                     (class Eq, class Show)



data LeuronSift
  = LeuronSift_Linear
  | LeuronSift_Random



derive instance genericLeuronSift :: Generic LeuronSift



instance leuronSiftEq :: Eq LeuronSift where eq = gEq
instance leuronSiftShow :: Show LeuronSift where show = gShow



-- instance leuronSiftEq :: Eq LeuronSift where
--  eq LeuronSift_Linear LeuronSift_Linear = true
--  eq LeuronSift_Random LeuronSift_Random = true
--  eq _                 _                 = false
