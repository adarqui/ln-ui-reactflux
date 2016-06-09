module LN.Eval.Boards (
  eval_GetBoards,
  eval_GetBoardsForForum,
  eval_Board
) where



import Data.Array                      (head, deleteAt, modifyAt, nub)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, bind, pure, ($), (<>))

import LN.Api                          (rd, getBoardPacks_ByForumId', getBoardPack', postBoard_ByForumId', putBoard')
import LN.Component.Types              (EvalEff)
import LN.Input.Board                  (InputBoard(..), Board_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Helpers.Map                  (idmapFrom)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.State.Loading                (l_currentBoard)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.T.Internal.Convert           (boardResponseToBoardRequest)
import LN.T                            (BoardPackResponses(..), BoardPackResponse(..)
                                       , ForumPackResponse(..)
                                       , _ForumPackResponse, forum_, _ForumResponse, name_
                                       , BoardResponse(..)
                                       , BoardRequest(..)
                                       , _BoardRequest, displayName_, description_, icon_, tags_, guard_
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_, name_)



eval_GetBoards :: EvalEff
eval_GetBoards eval (GetBoards next) = pure next



eval_GetBoardsForForum :: EvalEff
eval_GetBoardsForForum eval (GetBoardsForForum forum_id next) = do

  modify (_{ boards = (M.empty :: M.Map Int BoardPackResponse) })

  e_board_packs <- rd $ getBoardPacks_ByForumId' forum_id
  case e_board_packs of
    Left err -> eval (AddErrorApi "eval_GetBoardsForForum" err next)
    Right (BoardPackResponses board_packs) -> do

      let
        boards_map = idmapFrom (\(BoardPackResponse pack) -> pack.boardId) board_packs.boardPackResponses

      modify (_{ boards = boards_map })
      pure next



eval_Board :: EvalEff
eval_Board eval (CompBoard sub next) = do

  org_pack   <- gets _.currentOrganization
  forum_pack <- gets _.currentForum
  let
    org_name   = maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) org_pack
    forum_name = maybe "unknown" (\org -> org ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. name_) forum_pack

  case sub of
    InputBoard_Mod q -> do
      case q of
        SetDisplayName name -> mod $ set (\req -> _BoardRequest .. displayName_ .~ name $ req)

        SetDescription s    -> mod $ set (\req -> _BoardRequest .. description_ .~ Just s $ req)
        RemoveDescription   -> mod $ set (\req -> _BoardRequest .. description_ .~ Nothing $ req)

        SetIcon s           -> mod $ set (\req -> _BoardRequest .. icon_ .~ Just s $ req)
        RemoveIcon          -> mod $ set (\req -> _BoardRequest .. icon_ .~ Nothing $ req)

        Create forum_id     -> do

          m_req <- gets _.currentBoardRequest

          case m_req of

               Just req -> do

                 e_board <- rd $ postBoard_ByForumId' forum_id req

                 case e_board of
                      Left err                    -> eval (AddErrorApi "eval_Board(Create)::postBoard'" err next)
                      Right (BoardResponse board) -> eval (Goto (OrganizationsForumsBoards org_name forum_name (Show board.name) []) next)

               _        -> eval (AddError "eval_Board(Create)" "Board request doesn't exist" next)


        EditP board_id -> do

          m_req <- gets _.currentBoardRequest

          case m_req of
               Nothing  -> eval (AddError "eval_Board(Edit)" "Board request doesn't exist" next)
               Just req -> do

                 e_org <- rd $ putBoard' board_id req

                 case e_org of
                      Left err  -> eval (AddErrorApi "eval_Board(Edit)::putBoard" err next)
                      Right org -> do

                        modify (\st->st{ currentBoardRequest = Just $ boardResponseToBoardRequest org })
                        pure next

    _   -> pure next

 where
 append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
 append Nothing a    = Just [a]
 append (Just arr) a = Just $ nub $ arr <> [a]
 set v req           = Just (v req)
 mod new             = modify (\st->st{ currentBoardRequest = maybe Nothing new st.currentBoardRequest }) $> next
