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
import LN.View.Helpers                    ( glyphButtonDef_ArrowUp
                                          , glyphButtonDef_ArrowDown
                                          , glyphButtonDef_Minus
                                          , glyphButtonDef_Star
                                          , glyphButtonDef_StarEmpty
                                          )
import LN.T                               ( Ent
                                          , LikeOpt(..)
                                          , LikeResponse, _LikeResponse
                                          , StarResponse, _StarResponse
                                          , opt_
                                          )



renderLike :: Ent -> Int -> Maybe LikeResponse -> Maybe StarResponse -> ComponentHTML Input
renderLike ent ent_id m_like m_star =

  H.div [P.class_ B.row] [

    glyphButtonDef_ArrowUp colorLike $ CompLike (likeInput ent ent_id m_like),
    glyphButtonDef_Minus colorNeutral $ CompLike (neutralInput ent ent_id m_like),
    glyphButtonDef_ArrowDown colorDislike $ CompLike (dislikeInput ent ent_id m_like),

    (case m_star of
         Nothing   -> glyphButtonDef_StarEmpty colorStar
         Just star -> glyphButtonDef_Star colorStar) $ CompStar (InputStar ent ent_id m_star)
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

  starButton  =
    case m_star of
         Nothing -> B.glyphiconStarEmpty
         Just _  -> B.glyphiconStar

  likeInput   =
    case m_like of
         Nothing -> InputLike_Like
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Like -> InputLike_Un
                         _    -> InputLike_Like
  neutralInput =
    case m_like of
         Nothing -> InputLike_Neutral
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Neutral -> InputLike_Un
                         _       -> InputLike_Neutral
  dislikeInput =
    case m_like of
         Nothing -> InputLike_Dislike
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Dislike -> InputLike_Un
                         _       -> InputLike_Dislike
