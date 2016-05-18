module LN.View.Module.Like (
  renderLike
) where



import Data.Maybe                         (Maybe(..), maybe)
import Halogen                            (ComponentHTML)
import Halogen.HTML.Indexed               as H
import Halogen.HTML.Properties.Indexed    as P
import Halogen.HTML.Events.Indexed        as E
import Halogen.Themes.Bootstrap3          as B
import Halogen.HTML.CSS.Indexed           as HCSS
import CSS                                as HCSS
import Optic.Core                         ((^.), (..))
import Prelude                            (($), (<<<))

import LN.Input.Like                      (InputLike(..))
import LN.Input.Types                     (Input(..))
import LN.T                               ( Ent(..)
                                          , LikeOpt(..)
                                          , LikeResponse(..), _LikeResponse
                                          , StarResponse(..), _StarResponse
                                          , opt_
                                          )



renderLike :: Ent -> Int -> Maybe LikeResponse -> Maybe StarResponse -> ComponentHTML Input
renderLike ent ent_id mlike mstar =
  H.div [P.class_ B.row] [
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorLike,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLike (InputLike_Like ent ent_id mlike))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconArrowUp]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorNeutral,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLike (InputLike_Neutral ent ent_id mlike))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconMinus]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorDislike,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLike (InputLike_Dislike ent ent_id mlike))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconArrowDown]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorStar,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLike (InputLike_Star ent ent_id mstar))
      ] [H.span [P.classes [B.glyphicon, star]] []]
    ]
  ]
  where
  color c   = HCSS.style $ HCSS.color c
  colorLike =
    case mlike of
         Nothing -> color HCSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Like -> color HCSS.green
                         _    -> color HCSS.black
  colorNeutral =
    case mlike of
         Nothing -> color HCSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Neutral -> color HCSS.yellow
                         _       -> color HCSS.black
  colorDislike =
    case mlike of
         Nothing -> color HCSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Dislike -> color HCSS.red
                         _       -> color HCSS.black
  colorStar   =
    case mstar of
         Nothing -> color HCSS.black
         Just _  -> color HCSS.orange
  star        =
    case mstar of
         Nothing -> B.glyphiconStarEmpty
         Just _  -> B.glyphiconStar
