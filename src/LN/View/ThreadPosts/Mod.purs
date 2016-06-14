module LN.View.ThreadPosts.Mod (
  renderView_ThreadPosts_Delete,
  renderView_ThreadPosts_New,
  renderView_ThreadPosts_Edit,
  renderView_ThreadPosts_Mod,
  renderView_ThreadPosts_Mod'
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events.Indexed     as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Input.ThreadPost             (ThreadPost_Mod(..))
import LN.Input.Types                  (Input(..), cThreadPostMod)
import LN.State.Loading                (getLoading, l_currentThreadPost)
import LN.State.ThreadPost             (ThreadPostRequestState)
import LN.State.Types                  (State)
import LN.View.ThreadPosts.Shared      (postDataToBody)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_ThreadPosts_Delete :: Int -> State -> ComponentHTML Input
renderView_ThreadPosts_Delete threadPost_id st =

  case st.currentThreadPost, getLoading l_currentThreadPost st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "threadPost unavailable."]]
       Just pack, false -> renderView_ThreadPosts_Delete' pack st



renderView_ThreadPosts_Delete' :: ThreadPostPackResponse -> State -> ComponentHTML Input
renderView_ThreadPosts_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 threadPost = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse



renderView_ThreadPosts_New :: State -> ComponentHTML Input
renderView_ThreadPosts_New = renderView_ThreadPosts_Mod Nothing



renderView_ThreadPosts_Edit :: Int -> State -> ComponentHTML Input
renderView_ThreadPosts_Edit threadPost_id = renderView_ThreadPosts_Mod (Just threadPost_id)



renderView_ThreadPosts_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_ThreadPosts_Mod m_post_id st =
  case st.currentThread, st.currentThreadPostRequest, st.currentThreadPostRequestSt, getLoading l_currentThreadPost st.loading of
    _, _, _, true                                   -> renderLoading
    Just thread, Just post_req, Just post_st, false -> renderView_ThreadPosts_Mod' thread m_post_id post_req post_st
    _, _, _, false                                  -> H.div_ [H.p_ [H.text "ThreadPosts_Mod: unexpected error."]]



renderView_ThreadPosts_Mod' :: ThreadPackResponse -> Maybe Int -> ThreadPostRequest -> ThreadPostRequestState -> ComponentHTML Input
renderView_ThreadPosts_Mod' thread_pack m_post_id post_req post_st =
  H.div_ [

    H.h1_ [ H.text "Add ThreadPost" ]

  , H.li_ [
      H.div [P.class_ B.row] [
          H.div [P.class_ B.colXs2] [
            H.p_ [H.text "1"]
          ]
        , H.div [P.class_ B.colXs9] [
            H.p_ [H.text "2"]
          , H.div [P.class_ B.well] [
            -- TODO FIXME , need to fix this input form, doesnt do anything
                 H.a [P.href "#"] [H.text "Bold"]
              ,  H.a [P.href "#"] [H.text "Youtube"]
              ,  H.textarea [
                   P.class_ B.formControl,
                   P.rows 10,
                   P.value body,
                   E.onValueChange $ E.input (cThreadPostMod <<< SetBody)
                 ]
            -- TODO FIXME , need to style these buttons properly etc
              , H.a [
                  P.classes [B.btn, B.btnPrimary, B.pullRight, B.btnLg],
                  E.onClick $ E.input_ $
                    case m_post_id of
                         Nothing -> cThreadPostMod $ Create thread.id
                         Just post_id -> cThreadPostMod $ EditP post_id
                ] [H.text "send"]
              , H.a [
                  P.classes [B.btn, B.btnPrimary, B.pullRight, B.btnLg],
                  E.onClick $ E.input_ (cThreadPostMod RemoveBody)
                ] [H.text "cancel"]
              , H.a [P.classes [B.btn, B.btnPrimary, B.pullRight, B.btnLg]] [H.text "preview"]
            ]
          ]
        , H.div [P.class_ B.colXs1] [
            H.p_ [H.text "3"]
          ]
     ]
    ]

  ]
  where
  threadPost = unwrapThreadPostRequest post_req
  body       = postDataToBody $ post_req ^. _ThreadPostRequest .. body_
  thread     = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
