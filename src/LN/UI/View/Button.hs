{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Button (
  createButtonsCreateEditCancel,
  glyphButton
) where



import Data.Monoid ((<>))
import           Data.Int            (Int64)
import Data.Text (Text)
import           React.Flux

import qualified LN.UI.App.Route     as Route (Action (..), dispatch)
import           LN.UI.Router.Route  (RouteWith (..))
import           LN.UI.Types         (HTMLView_)
import           LN.UI.View.Internal
import LN.UI.Helpers.GHCJS (JSString)



createButtonsCreateEditCancel
  :: Maybe Int64
  -> ViewEventHandler -- ^ save handler
  -> (Int64 -> ViewEventHandler) -- ^ edit handler
  -> RouteWith -- ^ cancel route
  -> HTMLView_

createButtonsCreateEditCancel m_edit_id save_handler edit_handler cancel_route_with =
  div_ $ do
    save_or_edit
    createSimpleInfoButton "Cancel" (Route.dispatch $ Route.Goto cancel_route_with)
  where
  save_or_edit =
    case m_edit_id of
      Nothing      -> createSimpleInfoButton "Create" save_handler
      Just edit_id -> createSimpleInfoButton "Save" (edit_handler edit_id)



glyphButton :: JSString -> JSString -> Maybe Text -> [SomeStoreAction] -> HTMLView_
glyphButton glyph sz m_text click_handler =
  button_ [ "className" $= ("btn btn-default " <> sz)
          , onClick $ \_ _ -> click_handler
          ] $ do
            span_ [ "className" $= ("glyphicon " <> glyph)] text
  where
  text =
    case m_text of
      Nothing   -> mempty
      Just text -> elemText text
