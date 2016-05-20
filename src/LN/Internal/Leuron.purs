module LN.Internal.Leuron (
  LeuronSift (..)
) where



import Prelude (class Eq, eq)



data LeuronSift
  = LeuronSift_Linear
  | LeuronSift_Random



instance leuronSiftEq :: Eq LeuronSift where
  eq LeuronSift_Linear LeuronSift_Linear = true
  eq LeuronSift_Random LeuronSift_Random = true
  eq _                 _                 = false
