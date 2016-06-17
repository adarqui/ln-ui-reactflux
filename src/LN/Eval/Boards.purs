module LN.Eval.Boards (
  eval_Board
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, (:))
import Data.Ebyam                      (ebyam)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Data.String                     (toLower)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, ($), (<>), (<<<))

import LN.Api                          (rd, getBoardPacks_ByForumId', getBoardPack', postBoard_ByForumId', putBoard')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Input.Board                  (InputBoard(..), Board_Act(..), Board_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Helpers.Map                  (idmapFrom)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.State.Loading                (l_currentBoard, l_boards)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.T.Internal.Convert           (boardResponseToBoardRequest)
import LN.T                            (BoardPackResponses(..), BoardPackResponse(..)
                                       , ForumPackResponse(..)
                                       , _ForumPackResponse, forum_, _ForumResponse, name_, forumId_
                                       , BoardResponse(..)
                                       , _BoardResponse, name_
                                       , BoardRequest(..)
                                       , _BoardRequest, displayName_, description_, suggestedTags_, icon_, tags_, guard_
                                       , isAnonymous_, canCreateSubBoards_, canCreateThreads_
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_, name_)



eval_Board :: EvalEff
eval_Board eval (CompBoard sub next) = do

  org_pack   <- gets _.currentOrganization
  forum_pack <- gets _.currentForum
  let
    org_name   = maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_) org_pack
    forum_name = maybe "unknown" (\org -> org ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. name_) forum_pack

  case sub of

    InputBoard_Act q -> do
      case q of
        Gets                            -> pure next
        Gets_ByCurrentForum             -> act_gets_by_current_forum
        GetSid_ByCurrentForum forum_sid -> act_get_sid_by_current_forum forum_sid


    InputBoard_Mod q -> do
      case q of
        SetDisplayName name -> mod $ set (\req -> _BoardRequest .. displayName_ .~ name $ req)
        SetDescription s    -> mod $ set (\req -> _BoardRequest .. description_ .~ Just s $ req)
        RemoveDescription   -> mod $ set (\req -> _BoardRequest .. description_ .~ Nothing $ req)

        SetIsAnonymous b        -> mod $ set (\req -> _BoardRequest .. isAnonymous_ .~ b $ req)
        SetCanCreateSubBoards b -> mod $ set (\req -> _BoardRequest .. canCreateSubBoards_ .~ b $ req)
        SetCanCreateThreads b   -> mod $ set (\req -> _BoardRequest .. canCreateThreads_ .~ b $ req)

        SetIcon s           -> mod $ set (\req -> _BoardRequest .. icon_ .~ Just s $ req)
        RemoveIcon          -> mod $ set (\req -> _BoardRequest .. icon_ .~ Nothing $ req)

        SetSuggestedTag s   -> modSt $ (_{currentSuggestedTag = Just s})
        AddSuggestedTag     -> do
          m_req_st <- gets _.currentBoardRequestSt
          ebyam m_req_st (pure next) $ \req_st ->
            case req_st.currentSuggestedTag of
               Nothing  -> pure next
               Just tag -> do
                 mod $ set (\(BoardRequest req) -> BoardRequest req{suggestedTags = nub $ sort $ toLower tag : req.suggestedTags})
                 modSt $ (_{currentSuggestedTag = Nothing})
                 pure next
        DeleteSuggestedTag idx -> mod $ set (\(BoardRequest req) -> BoardRequest req{suggestedTags = maybe req.suggestedTags id $ deleteAt idx req.suggestedTags})
        ClearSuggestedTags     -> mod $ set (\req -> _BoardRequest .. suggestedTags_ .~ [] $ req)

        SetTag s             -> modSt $ (_{currentTag = Just s})
        AddTag               -> do
          m_req_st <- gets _.currentBoardRequestSt
          ebyam m_req_st (pure next) $ \req_st ->
            case req_st.currentTag of
               Nothing  -> pure next
               Just tag -> do
                 mod $ set (\(BoardRequest req) -> BoardRequest req{tags = nub $ sort $ toLower tag : req.tags})
                 modSt $ (_{currentTag = Nothing})
                 pure next
        DeleteTag idx        -> mod $ set (\(BoardRequest req) -> BoardRequest req{tags = maybe req.tags id $ deleteAt idx req.tags})
        ClearTags            -> mod $ set (\req -> _BoardRequest .. tags_ .~ [] $ req)

        Create forum_id     -> mod_create forum_id org_name forum_name
        EditP board_id      -> mod_edit board_id org_name forum_name


    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentBoardRequest = maybe Nothing new st.currentBoardRequest }) $> next
  modSt new           = modify (\st->st{ currentBoardRequestSt = maybe Nothing (Just <<< new) st.currentBoardRequestSt }) $> next



  act_gets_by_current_forum = do
    modify (_{ boards = (M.empty :: M.Map Int BoardPackResponse) })
    m_forum_pack <- gets _.currentForum
    case m_forum_pack of
      Nothing         -> eval (AddError "eval_Board(Act/Gets)" "Forum doesn't exist" next)
      Just forum_pack -> do
        modify $ setLoading l_boards
        e_board_packs <- rd $ getBoardPacks_ByForumId' (forum_pack ^. _ForumPackResponse .. forumId_)
        modify $ clearLoading l_boards
        case e_board_packs of
          Left err -> eval (AddErrorApi "eval_GetBoardsForForum" err next)
          Right (BoardPackResponses board_packs) -> do
            let
              boards_map = idmapFrom (\(BoardPackResponse pack) -> pack.boardId) board_packs.boardPackResponses
            modify (_{ boards = boards_map })
            pure next



  act_get_sid_by_current_forum board_sid = do
    modify (_{ currentBoard = Nothing })
    m_forum_pack <- gets _.currentForum
    case m_forum_pack of
      Nothing       -> eval (AddError "eval_Board(Act/Get)" "Forum doesn't exist" next)
      Just forum_pack -> do
        modify $ setLoading l_currentBoard
        e_board_pack <- rd $ ApiS.getBoardPack_ByForumId' board_sid (forum_pack ^. _ForumPackResponse .. forumId_)
        modify $ clearLoading l_currentBoard
        case e_board_pack of
          Left err         -> eval (AddErrorApi "eval_Board(Act/Get)::getBoardPacks_ByOrgName'" err next)
          Right board_pack -> do
            modify (_{ currentBoard = Just board_pack })
            pure next



  mod_create forum_id org_name forum_name = do
    m_req <- gets _.currentBoardRequest
    case m_req of
         Just req -> do
           e_board <- rd $ postBoard_ByForumId' forum_id req
           case e_board of
                Left err                    -> eval (AddErrorApi "eval_Board(Create)::postBoard'" err next)
                Right (BoardResponse board) -> eval (Goto (OrganizationsForumsBoards org_name forum_name (Show board.name) []) next)
         _        -> eval (AddError "eval_Board(Create)" "Board request doesn't exist" next)



  mod_edit board_id org_name forum_name = do
    m_req <- gets _.currentBoardRequest
    case m_req of
         Nothing  -> eval (AddError "eval_Board(Edit)" "Board request doesn't exist" next)
         Just req -> do
           e_board <- rd $ putBoard' board_id req
           case e_board of
                Left err    -> eval (AddErrorApi "eval_Board(Edit)::putBoard" err next)
                Right board -> do
                  modify (\st->st{ currentBoardRequest = Just $ boardResponseToBoardRequest board })
                  eval (Goto (OrganizationsForumsBoards org_name forum_name (Show $ board ^. _BoardResponse .. name_) []) next)
                  pure next
