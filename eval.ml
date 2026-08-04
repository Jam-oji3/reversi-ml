open Command
open Bitboard
(* 中盤用の打ち手選択 *)

type square_type = 
  | Corner
  | X_with_own_corner (*Xマスだが、対応する角が取れている*)
  | X_without_own_corner
  | C_with_own_corner (*Cマスだが、対応する角が取れている*)
  | C_without_own_corner
  | Other

(*角、X,Cマスの分類*)

let is_corner = function
  | (1, 1) | (8, 1) | (1, 8) | (8, 8) -> true
  | _ -> false
let is_other (i,j) =
  (3 <= i && i <= 6) || (3 <= j && j <= 6)

(*最も近い角を返す*)
let nearest_corner (i,j) =
  if (i <= 4 && j <= 4) then (1,1)
  else if (i >= 5 && j <= 4) then (8,1)
  else if (i <= 4 && j >= 5) then (1,8)
  else (8,8)
  

let classify_square pos board color =
  if is_corner pos then Corner
  else if is_other pos then Other
  else
    match pos with
    | (2,2) | (7,2) | (2,7) | (7,7) ->
        if color_at board (nearest_corner pos) = color then X_with_own_corner
        else X_without_own_corner
    | _ ->
        if color_at board (nearest_corner pos) = color then C_with_own_corner
        else C_without_own_corner

let score_square_type square_type =
  match square_type with
  | Corner -> 1.0
  | X_with_own_corner -> 0.6
  | C_with_own_corner -> 0.4
  | Other -> 0.0
  | C_without_own_corner -> -0.4
  | X_without_own_corner -> -1.0

(*現在の盤面boardにおけるcolor側の合法手数のアドバンテージ*)
let mobility board color =
  List.length (valid_moves board color)
  - List.length (valid_moves board (Color.opposite color))

let mobility_after_move board color pos =
  let new_board = do_move board (Mv pos) color in
  mobility new_board color

(*正規化mobility
終局局面では0.0*)
let norm_mobility board color =
  let mine = List.length (valid_moves board color) in
  let opponent = List.length (valid_moves board (Color.opposite color)) in
  let total = mine + opponent in
  if total = 0 then 0.0 else float_of_int (mine - opponent) /. float_of_int total

let potential_mobility_score board color =
  let mine = potential_mobility board color in
  let theirs = potential_mobility board (Color.opposite color) in
  let total = mine + theirs in
  if total = 0 then 0.0 else float_of_int (mine - theirs) /. float_of_int total

let normalized_mobility_after_move board color pos =
  let new_board = do_move board (Mv pos) color in
  norm_mobility new_board color

  (*
type weights = {
  square_type_weight : float;
  mobility_weight : float;
}

let w = {
  square_type_weight = 1.0;
  mobility_weight = 1.0;
}

let eval_move board color pos = 
  let square_score = score_square_type (classify_square pos board color) in
  let mobility_score = normalized_mobility_after_move board color pos in
  w.square_type_weight *. square_score +. w.mobility_weight *. mobility_score

(*手の比較*)
let better_move board color pos1 pos2 =
  eval_move board color pos1 > eval_move board color pos2
*)
let corners = [ (1, 1); (8, 1);
  (1, 8); (8, 8) ]

(* 盤面評価用の X/C マス。角は [corner_score] で別に評価する。 *)
let x_squares = [ (2, 2); (7, 2); (2, 7); (7, 7) ]

let c_squares =
  [ (2, 1); (1, 2); (7, 1); (8, 2);
    (1, 7); (2, 8); (7, 8); (8, 7) ]

(*自分が確保した角に対するX/Cは加点
そうでないなら減点*)
let square_list_score board color squares =
  let opp = Color.opposite color in
  List.fold_left
    (fun total pos ->
      let square_color = color_at board pos in
      if square_color = color then
        total +. score_square_type (classify_square pos board color)
      else if square_color = opp then
        total -. score_square_type (classify_square pos board opp)
      else total)
    0.0
    squares

let x_score board color = square_list_score board color x_squares

let c_score board color = square_list_score board color c_squares

let corner_score board color =
  let opp = Color.opposite
  color in
  let diff =
    List.fold_left
      (fun total corner ->
        if color_at board corner
        = color then total + 1
        else if color_at board
        corner = opp then
        total - 1
        else total)
      0
      corners
  in
  float_of_int diff /. 4.0

(* 次の一手でとれる角の数 *)
let corner_move_count board color =
  List.length (List.filter is_corner (valid_moves board color))

let corner_access_score board color =
  let mine = corner_move_count board color in
  let theirs = corner_move_count board (Color.opposite color) in
  float_of_int (mine - theirs) /. 4.0

(* stable discの近似: 角からつながった同色をカウントする *)
let stable_edge_positions board color =
  let runs =
    [ ((1, 1), (1, 0)); ((1, 1), (0, 1));
      ((8, 1), (-1, 0)); ((8, 1), (0, 1));
      ((1, 8), (1, 0)); ((1, 8), (0, -1));
      ((8, 8), (-1, 0)); ((8, 8), (0, -1)) ]
  in
  let rec collect (i, j) (di, dj) positions =
    if i < 1 || i > 8 || j < 1 || j > 8 || color_at board (i, j) <> color then
      positions
    else
      collect (i + di, j + dj) (di, dj) ((i, j) :: positions)
  in
  List.sort_uniq compare
    (List.concat_map (fun (corner, direction) -> collect corner direction []) runs)

let stable_disc_count board color =
  List.length (stable_edge_positions board color)

let stable_disc_score board color =
  let mine = stable_disc_count board color in
  let theirs = stable_disc_count board (Color.opposite color) in
  float_of_int (mine - theirs) /. 28.0

let is_edge_non_corner pos =
  not (is_corner pos)
  && match pos with
     | (i, j) -> i = 1 || i = 8 || j = 1 || j = 8

let edge_neighbors = function
  | (i, j) when j = 1 || j = 8 -> [ (i - 1, j); (i + 1, j) ]
  | (i, j) when i = 1 || i = 8 -> [ (i, j - 1); (i, j + 1) ]
  | _ -> invalid_arg "edge_neighbors: position is not on an edge"

let is_edge_hole board pos =
  List.for_all (fun neighbor -> color_at board neighbor <> Color.none)
    (edge_neighbors pos)

(*
相手の辺上への手を警戒する
通常の辺手: 1
辺上で両隣が埋まっているとき: 2
着手後に角アクセスを与える: 5
重みに加える
*)
let edge_threat_value board color =
  let corner_moves_before = corner_move_count board color in
  List.fold_left
    (fun total pos ->
      let hole_bonus = if is_edge_hole board pos then 2 else 0 in
      let next_board = do_move board (Mv pos) color in
      let corner_moves_after = corner_move_count next_board color in
      let corner_bonus = if corner_moves_after > corner_moves_before then 5 else 0 in
      total + 1 + hole_bonus + corner_bonus)
    0
    (List.filter is_edge_non_corner (valid_moves board color))

let edge_threat_score board color =
  let mine = edge_threat_value board color in
  let theirs = edge_threat_value board (Color.opposite color) in
  let total = mine + theirs in
  if total = 0 then 0.0 else float_of_int (mine - theirs) /. float_of_int total

let disc_diff_score board color =
  let opp = Color.opposite color in
  let diff = count board color - count board opp in
  float_of_int diff /. 64.0

let all_positions =
  let rec loop i j positions =
    if j = 9 then List.rev positions
    else if i = 9 then loop 1 (j + 1) positions
    else loop (i + 1) j ((i, j) :: positions)
  in
  loop 1 1 []

(* Frontier: 空きマスに接している石*)
let is_frontier board (i, j) =
  color_at board (i, j) <> Color.none
  && List.exists
       (fun (di, dj) ->
         let next = (i + di, j + dj) in
         match next with
         | (x, y) when x < 1 || x > 8 || y < 1 || y > 8 -> false
         | _ -> color_at board next = Color.none)
       dirs

let frontier_count board color =
  List.fold_left
    (fun total pos ->
      if color_at board pos = color && is_frontier board pos then
        total + 1
      else
        total)
    0
    all_positions

let frontier_score board color =
  let opp = Color.opposite color in
  let mine = frontier_count board color in
  let theirs = frontier_count board opp in
  let total = mine + theirs in
  if total = 0 then 0.0
  else float_of_int (theirs - mine) /. float_of_int total (*両者のfrontierの合計で割る*)

type weights = {
  x_weight : float;
  c_weight : float;
  mobility_weight : float;
  potential_mobility_weight : float;
  frontier_weight : float;
  corner_weight : float;
  corner_access_weight : float;
  stable_disc_weight : float;
  edge_threat_weight : float;
  disc_diff_weight : float;
}

(* 局面依存で重みを変える *)
let weights_for board =
  let empty = empty_count board in
  if empty >= 44 then
    {
      x_weight = 1.5;
      c_weight = 0.7;
      corner_weight = 8.0;
      corner_access_weight = 6.0;
      stable_disc_weight = 2.0;
      edge_threat_weight = 1.0;
      mobility_weight = 4.0;
      potential_mobility_weight = 1.5;
      frontier_weight = 1.0;
      disc_diff_weight = -0.5;
    }
  else if empty >= 20 then
    {
      x_weight = 0.8;
      c_weight = 0.4;
      corner_weight = 12.0;
      corner_access_weight = 5.0;
      stable_disc_weight = 6.0;
      edge_threat_weight = 2.0;
      mobility_weight = 3.0;
      potential_mobility_weight = 0.8;
      frontier_weight = 0.8;
      disc_diff_weight = 0.0;
    }
  else
    {
      x_weight = 0.2;
      c_weight = 0.1;
      corner_weight = 16.0;
      corner_access_weight = 2.0;
      stable_disc_weight = 10.0;
      edge_threat_weight = 0.3;
      mobility_weight = 1.5;
      potential_mobility_weight = 0.2;
      frontier_weight = 0.3;
      disc_diff_weight = 8.0;
    }


let eval_board board color =
    let w = weights_for board in
    w.x_weight *. x_score board color +.
    w.c_weight *. c_score board color +.
    w.corner_weight *. corner_score board color +.
    w.corner_access_weight *. corner_access_score board color +.
    w.stable_disc_weight *. stable_disc_score board color +.
    w.edge_threat_weight *. edge_threat_score board color +.
    w.mobility_weight *. norm_mobility board color +.
    w.potential_mobility_weight *. potential_mobility_score board color +.
    w.frontier_weight *. frontier_score board color +.
    w.disc_diff_weight *. disc_diff_score board color
