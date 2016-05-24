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
import CSS                                as CSS
import Optic.Core                         ((^.), (..))
import Prelude                            (($), (<<<))

import LN.Input.Like                      (InputLike(..))
import LN.Input.Star                      (InputStar(..))
import LN.Input.Types                     (Input(..))
import LN.T                               ( Ent
                                          , LikeOpt(..)
                                          , LikeResponse, _LikeResponse
                                          , StarResponse, _StarResponse
                                          , opt_
                                          )



renderLike :: Ent -> Int -> Maybe LikeResponse -> Maybe StarResponse -> ComponentHTML Input
renderLike ent ent_id m_like m_star =
  H.div [P.class_ B.row] [
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorLike,
        P.classes [B.btn, B.btnDefault, B.btnLg],
        E.onClick $ E.input_ $ (CompLike (InputLike_Like ent ent_id m_like))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconArrowUp]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorNeutral,
        P.classes [B.btn, B.btnDefault, B.btnLg],
        E.onClick $ E.input_ $ (CompLike (InputLike_Neutral ent ent_id m_like))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconMinus]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorDislike,
        P.classes [B.btn, B.btnDefault, B.btnLg],
        E.onClick $ E.input_ $ (CompLike (InputLike_Dislike ent ent_id m_like))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconArrowDown]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorStar,
        P.classes [B.btn, B.btnDefault, B.btnLg],
        E.onClick $ E.input_ $ (CompStar (InputStar ent ent_id m_star))
      ] [H.span [P.classes [B.glyphicon, star]] []]
    ]
  ]
  where
  color c   = HCSS.style $ CSS.color c
  colorLike =
    case m_like of
         Nothing -> color CSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Like -> color CSS.green
                         _    -> color CSS.black
  colorNeutral =
    case m_like of
         Nothing -> color CSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Neutral -> color CSS.yellow
                         _       -> color CSS.black
  colorDislike =
    case m_like of
         Nothing -> color CSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Dislike -> color CSS.red
                         _       -> color CSS.black
  colorStar   =
    case m_star of
         Nothing -> color CSS.black
         Just _  -> color CSS.orange
  star        =
    case m_star of
         Nothing -> B.glyphiconStarEmpty
         Just _  -> B.glyphiconStar
