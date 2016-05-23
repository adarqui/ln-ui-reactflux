module LN.Component where


import Halogen                     hiding (set)
import Prelude                     (pure)

import LN.Layout                   as L
import LN.Component.Types          (LNEff)
import LN.Input.Types              (Input(..))
import LN.State.Types              (State)
import LN.View
import LN.Eval.Goto
import LN.Eval.Errors
import LN.Eval.Me
import LN.Eval.Users
import LN.Eval.Organizations
import LN.Eval.Teams
import LN.Eval.Forums
import LN.Eval.Boards
import LN.Eval.Threads
import LN.Eval.ThreadPosts
import LN.Eval.Resources
import LN.Eval.Leurons
import LN.Eval.Socket
import LN.Eval.Nop

-- Components
import LN.Eval.CreateThread
import LN.Eval.Like
import LN.Eval.Star
import LN.Eval.OrderBy
import LN.Eval.ThreadPost
import LN.Eval.Profile



ui :: forall eff. {-Partial =>-} Component State Input (LNEff eff)
ui = component render eval

  where
  render state =
    L.defaultLayout state
      [ renderView state.currentPage state
      ]

  eval :: Eval Input State Input (LNEff eff)

  eval z@(Goto _ _)                                   = eval_Goto eval z

  eval z@(AddError _ _ _)                             = eval_AddError eval z
  eval z@(AddErrorF _ _ _)                            = eval_AddErrorF eval z
  eval z@(AddErrorApi _ _ _)                          = eval_AddErrorApi eval z
  eval z@(DelError _ _)                               = eval_DelError eval z
  eval z@(ClearErrors _)                              = eval_ClearErrors eval z

  eval z@(GetMe _)                                    = eval_GetMe eval z

  eval z@(GetUsers _)                                 = eval_GetUsers eval z

  eval z@(GetUser _ _)                                = eval_GetUser eval z

  eval z@(GetUsers_MergeMap_ByUser _ _)               = eval_GetUsers_MergeMap_ByUser eval z
  eval z@(GetUsers_MergeMap_ByUserId _ _)             = eval_GetUsers_MergeMap_ByUserId eval z

  eval z@(GetOrganizations _)                         = eval_GetOrganizations eval z
  eval z@(GetOrganization _ _)                        = eval_GetOrganization eval z
  eval z@(GetOrganizationForum _ _ _)                 = eval_GetOrganizationForum eval z
  eval z@(GetOrganizationForumBoard _ _ _ _)          = eval_GetOrganizationForumBoard eval z
  eval z@(GetOrganizationForumBoardThread _  _ _ _ _) = eval_GetOrganizationForumBoardThread eval z

  eval z@(GetTeams _)                                 = eval_GetTeams eval z

  eval z@(GetForums _)                                = eval_GetForums eval z
  eval z@(GetForumsForOrg _ _)                        = eval_GetForumsForOrg eval z

  eval z@(GetBoards _)                                = eval_GetBoards eval z
  eval z@(GetBoardsForForum _ _)                      = eval_GetBoardsForForum eval z

  eval z@(GetThreads _)                               = eval_GetThreads eval z
  eval z@(GetThreadsForBoard _ _)                     = eval_GetThreadsForBoard eval z

  eval z@(GetThreadPosts _)                           = eval_GetThreadPosts eval z
  eval z@(GetThreadPostsForThread _ _)                = eval_GetThreadPostsForThread eval z
  eval z@(GetThreadPost _ _)                          = eval_GetThreadPost eval z

  eval (GetThreadPostLikes next)                      = pure next

  eval (GetPMs next)                                  = pure next

  eval z@(GetResources _)                             = eval_GetResources eval z
  eval z@(GetResourceId _ _)                          = eval_GetResourceId eval z
  eval z@(GetResourceSid _ _)                         = eval_GetResourceSid eval z

  eval z@(GetResourcesLeurons _ _)                    = eval_GetResources eval z
  eval z@(GetResourceLeuronRandom _ _)                = eval_GetResourceLeuronRandom eval z
  eval z@(GetResourcesSiftLeurons _ _)                = eval_GetResourcesSiftLeurons eval z

  eval z@(GetLeurons _)                               = eval_GetLeurons eval z
  eval z@(GetLeuronId _ _)                            = eval_GetLeuronId eval z
  eval z@(GetLeuronSid _ _)                           = eval_GetLeuronSid eval z
  eval z@(GetLeuronRandom _)                          = eval_GetLeuronRandom eval z

  eval z@(ConnectSocket _)                            = eval_ConnectSocket eval z

  -- Components

  eval z@(CompThreadPost _ _)                         = eval_ThreadPost eval z
  eval z@(CompCreateThread _ _)                       = eval_CreateThread eval z
  eval z@(CompLike _ _)                               = eval_Like eval z
  eval z@(CompStar _ _)                               = eval_Star eval z
  eval z@(CompOrderBy _ _)                            = eval_OrderBy eval z
  eval z@(CompProfile _ _)                            = eval_Profile eval z

  eval z@(Nop _)                                      = eval_Nop eval z
