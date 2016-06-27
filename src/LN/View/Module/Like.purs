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
import LN.View.Helpers
import LN.T                               ( Ent
                                          , LikeOpt(..)
                                          , LikeResponse, _LikeResponse
                                          , StarResponse, _StarResponse
                                          , opt_
                                          )



renderLike :: Ent -> Int -> Maybe LikeResponse -> Maybe StarResponse -> ComponentHTML Input
renderLike ent ent_id m_like m_star =

  H.div [P.class_ B.row] [

    button_like colorLike $ CompLike (likeInput ent ent_id m_like),
    button_neutral colorNeutral $ CompLike (neutralInput ent ent_id m_like),
    button_dislike colorDislike $ CompLike (dislikeInput ent ent_id m_like),

    (case m_star of
         Nothing   -> button_starEmpty colorStar
         Just star -> button_star colorStar) $ CompStar (InputStar ent ent_id m_star)
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
