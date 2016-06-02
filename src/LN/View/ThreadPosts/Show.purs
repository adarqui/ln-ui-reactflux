module LN.View.ThreadPosts.Show (
  renderView_ThreadPosts_ShowI
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Input.Thread                  (Thread_Mod(..))
import LN.Input.Types                  (Input(..), cThreadMod)
import LN.State.Loading                (getLoading, l_currentThread)
import LN.State.Thread                  (ThreadRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_ThreadPosts_ShowI :: Int -> State -> ComponentHTML Input
renderView_ThreadPosts_ShowI post_id st = H.div_ [H.text "showI"]

--  case st.currentThread, getLoading l_currentThread st.loading of
--       _, true          -> renderLoading
--       Nothing, false   -> H.div_ [H.p_ [H.text "thread unavailable."]]
--       Just pack, false -> renderView_Threads_Delete' pack st



-- renderView_Threads_Delete' :: ThreadPackResponse -> State -> ComponentHTML Input
-- renderView_Threads_Delete' pack st =
--  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
-- where
-- thread = pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
