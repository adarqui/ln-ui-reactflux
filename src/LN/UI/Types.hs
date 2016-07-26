{-# LANGUAGE PatternSynonyms #-}

module LN.UI.Types (
  Array,
  List,
  Number,
  Tuple,
  tuple,
  String,
  Int,
  pattern Tuple,
  pattern Cons,
  pattern Nil,
  HTMLView,
  HTMLView_
) where



import           Prelude
import React.Flux



type Array a   = [a]
type List a    = [a]
type Number    = Double
type Tuple a b = (a, b)



tuple :: a -> b -> (a, b)
tuple = (,)


pattern Tuple a b = (a, b)
pattern Cons a a' = (:) a a'
pattern Nil       = []



-- type HTML = ReactElementM EventHandler
-- type HTML' = HTML ()

type HTMLView  = ReactElementM ViewEventHandler
type HTMLView_ = HTMLView_
