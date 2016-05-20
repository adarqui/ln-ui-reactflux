module LN.Input.Leuron (
  InputLeuron (..)
) where



import LN.Internal.Leuron (LeuronSift)



data InputLeuron
  = InputLeuron_Nop1
  | InputLeuron_Set_Sift LeuronSift
  | InputLeuron_Nop
