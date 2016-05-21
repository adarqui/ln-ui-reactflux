module LN.Internal.Leuron (
  LeuronSift (..)
) where



import Data.Generic                (class Generic)
import Prelude                     (class Eq, class Show, eq, show)



data LeuronSift
  = LeuronSift_Linear
  | LeuronSift_Random



derive instance genericLeuronSift :: Generic LeuronSift



instance leuronSiftEq :: Eq LeuronSift where
  eq LeuronSift_Linear LeuronSift_Linear = true
  eq LeuronSift_Random LeuronSift_Random = true
  eq _                 _                 = false



instance leuronSiftShow :: Show LeuronSift where
  show LeuronSift_Linear = "Linear"
  show LeuronSift_Random = "Random"
