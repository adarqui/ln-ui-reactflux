module LN.UI.Types (
  Array,
  List,
  Number,
  Tuple,
  tuple,
  String,
  Int
) where



import Prelude



type Array a   = [a]
type List a    = [a]
type Number    = Double
type Tuple a b = (a, b)



tuple :: a -> b -> (a, b)
tuple = (,)
