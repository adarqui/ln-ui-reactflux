module LN.Component.CreateThread (
  Comp_CreateThread_State,
  defaultCompThreadState
) where



type Comp_CreateThread_State = { name :: String }



defaultCompThreadState :: Comp_CreateThread_State
defaultCompThreadState = { name: "" }
