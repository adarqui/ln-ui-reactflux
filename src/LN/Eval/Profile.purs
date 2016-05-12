module LN.Eval.Profile (
  eval_Profile,
  eval_Profile_Setter
) where


import Data.Date.Helpers  (dateFromString)
import Data.Maybe.Unsafe  (fromJust)
import Halogen            (get, gets, modify)

import LN.Component.Types (EvalEff)
import LN.Input.Profile   (InputProfile(..))
import LN.Input.Types     (Input(..))
import LN.Api
import LN.T



eval_Profile :: EvalEff



eval_Profile eval (CompProfile InputProfile_Nop next) = do
  pure next



eval_Profile eval (CompProfile InputProfile_Post next) = do

  mme <- gets _.me

  case mme of
       Nothing -> pure next
       Just me -> do
         let
           profile_id  = me ^. _UserPackResponse .. userProfile_ ^. _ProfileResponse .. id_
           profile_req = (profileResponseToRequest $ me ^. _UserPackResponse .. userProfile_)

-- TODO FIXME API         rd $ putUserProfile' profile_id profile_req
         pure next




eval_Profile eval (CompProfile (InputProfile_Gender gender) next) = do

  eval_Profile_Setter gender_ GenderMale next



eval_Profile eval (CompProfile (InputProfile_Birthdate birthdate) next) = do

  eval_Profile_Setter birthdate_ (fromJust $ dateFromString birthdate) next



eval_Profile eval (CompProfile (InputProfile_Website mwebsite) next) = do

  eval_Profile_Setter website_ mwebsite next



eval_Profile eval (CompProfile (InputProfile_Location mlocation) next) = do

  eval_Profile_Setter location_ mlocation next



eval_Profile eval (CompProfile (InputProfile_Signature msignature) next) = do

  eval_Profile_Setter signature_ msignature next



eval_Profile_Setter accessor value next = do
  mme <- gets _.me

  case mme of
       Nothing -> pure next
       Just me -> do
          let
            profile = (_ProfileResponse .. accessor .~ value) $ (me ^. _UserPackResponse .. userProfile_)
            me' = (_UserPackResponse .. userProfile_ .~ profile) $ me
          modify (_ { me = Just me' })
          pure next
