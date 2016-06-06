module LN.Eval.Profile (
  eval_Profile,
  eval_Profile_Setter
) where


import Data.Date.Helpers  (dateFromString)
import Data.Maybe         (Maybe(..))
import Data.Maybe.Unsafe  (fromJust)
import Halogen            (gets, modify)
import Optic.Core         ((^.),(..), (.~))
import Prelude            (bind, pure, ($))

import LN.Component.Types (EvalEff)
import LN.Input.Profile   (InputProfile(..))
import LN.Input.Types     (Input(..))
import LN.Api             (rd, putUserProfile')
import LN.T               (UserPackResponse(..), _UserPackResponse
                          , ProfileResponse(..), _ProfileResponse
                          , ProfileGender(..)
                          , profile_, id_, gender_, birthdate_, signature_, website_, location_
                          , profileResponseToProfileRequest)



eval_Profile :: EvalEff



eval_Profile eval (CompProfile InputProfile_Nop next) = pure next



eval_Profile eval (CompProfile InputProfile_Post next) = do

  m_me <- gets _.me

  case m_me of
       Nothing -> eval (AddError "eval_Profile" "st.me doesn't exist" next)
       Just me -> do
         let
           profile_id  = me ^. _UserPackResponse .. profile_ ^. _ProfileResponse .. id_
           profile_req = (profileResponseToProfileRequest $ me ^. _UserPackResponse .. profile_)

         rd $ putUserProfile' profile_id profile_req
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
  m_me <- gets _.me

  case m_me of
       Nothing -> pure next
       Just me -> do
          let
            profile = (_ProfileResponse .. accessor .~ value) $ (me ^. _UserPackResponse .. profile_)
            me' = (_UserPackResponse .. profile_ .~ profile) $ me
          modify (_ { me = Just me' })
          pure next
