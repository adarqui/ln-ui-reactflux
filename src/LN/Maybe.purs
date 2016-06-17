module LN.Maybe (
  ebyam
) where



import Data.Maybe (Maybe(..), maybe)



-- maybe :: forall a b. b -> (a -> b) -> Maybe a -> b

ebyam :: forall a b. Maybe a -> b -> (a -> b) -> b
ebyam m_a b f_ab = maybe b f_ab m_a
