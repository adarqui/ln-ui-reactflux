module LN.View.Module.LikeThreadPost (
  renderLikeThreadPost
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

import LN.Input.LikeThreadPost            (InputLikeThreadPost(..))
import LN.Input.Types                     (Input(..))
import LN.T



renderLikeThreadPost :: Int -> ThreadPostPackResponse -> ComponentHTML Input
renderLikeThreadPost thread_post_id pack =
  H.div [P.class_ B.row] [
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorLike,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLikeThreadPost (InputLikeThreadPost_Like pack))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconArrowUp]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorNeutral,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLikeThreadPost (InputLikeThreadPost_Neutral pack))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconMinus]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorDislike,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLikeThreadPost (InputLikeThreadPost_Dislike pack))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconArrowDown]] []]
    ],
    H.span [P.class_ B.inputGroupBtn] [
      H.button [
        colorStar,
        P.classes [B.btn, B.btnDefault],
        E.onClick $ E.input_ $ (CompLikeThreadPost (InputLikeThreadPost_Star pack))
      ] [H.span [P.classes [B.glyphicon, B.glyphiconStar]] []]
    ]
  ]
  where
  color c   = HCSS.style $ HCSS.color c
  colorLike =
    case (pack ^. _ThreadPostPackResponse .. like_) of
         Nothing -> color HCSS.black
         Just r  -> case (r ^. _ThreadPostLikeResponse .. opt_) of
                         Like -> color HCSS.green
                         _    -> color HCSS.black
  colorNeutral =
    case (pack ^. _ThreadPostPackResponse .. like_) of
         Nothing -> color HCSS.black
         Just r  -> case (r ^. _ThreadPostLikeResponse .. opt_) of
                         Neutral -> color HCSS.yellow
                         _       -> color HCSS.black
  colorDislike =
    case (pack ^. _ThreadPostPackResponse .. like_) of
         Nothing -> color HCSS.black
         Just r  -> case (r ^. _ThreadPostLikeResponse .. opt_) of
                         Dislike -> color HCSS.red
                         _       -> color HCSS.black
  colorStar   = color HCSS.orange
