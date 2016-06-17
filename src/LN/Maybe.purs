module LN.Maybe (
  flippedMaybe
) where



import Data.Maybe (Maybe(..), maybe)



-- maybe :: forall a b. b -> (a -> b) -> Maybe a -> b

flippedMaybe :: forall a b. Maybe a -> b -> (a -> b) -> b
flippedMaybe m_a b f_ab = maybe b f_ab m_a
