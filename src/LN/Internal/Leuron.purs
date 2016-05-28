module LN.Internal.Leuron (
  LeuronSift (..),
  leuronToTyLeuron,
  defaultLeuronRequest,
  defaultFact,
  defaultFactList,
  defaultCard,
  defaultDCard
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(..))
import Prelude                     (class Eq, class Show, eq, show)

import LN.T                        (LeuronRequest, mkLeuronRequest, LeuronData(..), TyLeuron(..)
                                   , mkFact, Fact
                                   , mkFactList, FactList
                                   , mkCard, Card
                                   , mkDCard, DCard)



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



leuronToTyLeuron :: LeuronData -> TyLeuron
leuronToTyLeuron ld =
  case ld of
       _ -> TyLnEmpty



defaultLeuronRequest :: LeuronRequest
defaultLeuronRequest = mkLeuronRequest LnEmpty Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing



defaultFact :: Fact
defaultFact = mkFact ""



defaultFactList :: FactList
defaultFactList = mkFactList "" []



defaultCard :: Card
defaultCard = mkCard "" ""



defaultDCard :: DCard
defaultDCard = mkDCard "" ""
