module LN.Component where


import Halogen                     hiding (set)
import Prelude                     (pure)

import LN.Layout                   as L
import LN.Component.Types          (LNEff)
import LN.Input.Types              (Input(..))
import LN.State.Types
import LN.View
import LN.Eval.Goto
import LN.Eval.GetMe
import LN.Eval.GetUsers
import LN.Eval.GetOrganizations
import LN.Eval.GetTeams
import LN.Eval.GetForums
import LN.Eval.GetBoardPacks
import LN.Eval.GetThreadPacks
import LN.Eval.GetThreadPosts
import LN.Eval.Socket
import LN.Eval.Nop

-- Components
import LN.Eval.CreateThread
import LN.Eval.LikeThreadPost
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

    eval z@(Goto route next) = eval_Goto eval z

    eval z@(GetMe next) = eval_GetMe eval z

    eval z@(GetUsers next) = eval_GetUsers eval z

    eval z@(GetUser user_nick next) = eval_GetUser eval z

    eval z@(GetUsers_MergeMap_ByUser users next) = eval_GetUsers_MergeMap_ByUser eval z
    eval z@(GetUsers_MergeMap_ByUserId user_ids next) = eval_GetUsers_MergeMap_ByUserId eval z

    eval z@(GetOrganizations next) = eval_GetOrganizations eval z
    eval z@(GetOrganization _ _) = eval_GetOrganization eval z
    eval z@(GetOrganizationForum _ _ _) = eval_GetOrganizationForum eval z
    eval z@(GetOrganizationForumBoard _ _ _ _) = eval_GetOrganizationForumBoard eval z
    eval z@(GetOrganizationForumBoardThread _  _ _ _ _) = eval_GetOrganizationForumBoardThread eval z

    eval z@(GetTeams next) = eval_GetTeams eval z

    eval z@(GetForums next) = eval_GetForums eval z
    eval z@(GetForumsForOrg org_name next) = eval_GetForumsForOrg eval z

    eval z@(GetBoardPacks next) = eval_GetBoardPacks eval z
    eval z@(GetBoardPacksForForum forum next) = eval_GetBoardPacksForForum eval z

    eval z@(GetThreadPacks next) = eval_GetThreadPacks eval z
    eval z@(GetThreadPacksForBoard board next) = eval_GetThreadPacksForBoard eval z

    eval z@(GetThreadPosts next) = eval_GetThreadPosts eval z
    eval z@(GetThreadPostsForThread thread_name next) = eval_GetThreadPostsForThread eval z
    eval z@(GetThreadPost thread_post_id next)  = eval_GetThreadPost eval z

    eval (GetThreadPostLikes next)  = pure next

    eval (GetPMs next) = pure next

    eval (GetResources next) = pure next

    eval (GetLeurons next) = pure next

    eval z@(ConnectSocket next) = eval_ConnectSocket eval z

    -- Components

    eval z@(CompThreadPost sub next) = eval_ThreadPost eval z
    eval z@(CompCreateThread sub next) = eval_CreateThread eval z
    eval z@(CompLikeThreadPost sub next) = eval_LikeThreadPost eval z
    eval z@(CompOrderBy sub next) = eval_OrderBy eval z
    eval z@(CompProfile sub next) = eval_Profile eval z

    eval z@(Nop next) = eval_Nop eval z
