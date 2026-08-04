open Bitboard
open Command

let infty = 1_000_000.0

(* メモ化：
   αβ探索中の値は厳密値とは限らないため、値が上限・下限・厳密値のいずれかも一緒に記録する。 *)
type bound = Exact | Lower | Upper

type entry = {
  depth : int;
  score : float;
  bound : bound;
  best_move : Pos.t option;
}

module Position_key = struct
  type t = board * Color.t

  let equal (board1, color1) (board2, color2) =
    color1 = color2 && board1 = board2

  let hash = Hashtbl.hash
end

module Transposition_table = Hashtbl.Make (Position_key)

let cached_entry table board color =
  Transposition_table.find_opt table (board, color)

(* 前回の最善手を先に読む *)
let order_moves ?preferred board color moves =
  let score pos =
    Eval.score_square_type (Eval.classify_square pos board color)
  in
  let ordered = List.sort (fun pos1 pos2 -> compare (score pos2) (score pos1)) moves in
  match preferred with
  | None -> ordered
  | Some best_pos ->
      let best, rest = List.partition (fun pos -> pos = best_pos) ordered in
      best @ rest

type probe_result =
    | Cut of float
    | Search of float * float *
    Pos.t option

let probe table board color depth alpha beta =
  match cached_entry table board color with
  | Some { depth = cached_depth; score; bound; best_move }
    when cached_depth >= depth ->
      (match bound with
       | Exact -> Cut score
       | Lower ->
           let alpha = max alpha score in
           if alpha >= beta then Cut score else Search (alpha, beta, best_move)
       | Upper ->
           let beta = min beta score in
           if alpha >= beta then Cut score else Search (alpha, beta, best_move))
  | Some { best_move; _ } -> Search (alpha, beta, best_move)
  | None -> Search (alpha, beta, None)

let store table board color depth alpha beta score best_move =
  let bound =
    if score <= alpha then Upper
    else if score >= beta then Lower
    else Exact
  in
  let entry = { depth; score; bound; best_move } in
  match cached_entry table board color with
  | Some old when old.depth > depth -> ()
  | _ -> Transposition_table.replace table (board, color) entry

let rec negamax ~eval ~table ~deadline ~nodes board color depth alpha beta =
  Time_control.check deadline nodes;
  let alpha_original = alpha in
  let beta_original = beta in
  match probe table board color depth alpha beta with
  | Cut score -> score
  | Search (alpha, beta, preferred_move) ->
      let score, best_move =
        if depth = 0 then (eval board color, None)
        else
          let moves =
            order_moves ?preferred:preferred_move board color (valid_moves board color)
          in
          match moves with
          | [] ->
              let opp = Color.opposite color in
              if valid_moves board opp = [] then (eval board color, None)
              else
                (-.negamax ~eval ~table ~deadline ~nodes board opp depth (-.beta) (-.alpha), None)
          | _ ->
              let opp = Color.opposite color in
              let rec search_moves best_score best_pos alpha = function
                | [] -> (best_score, Some best_pos)
                | pos :: rest ->
                    let next_board = do_move board (Mv pos) color in
                    let score =
                      -.negamax ~eval ~table ~deadline ~nodes next_board opp
                        (depth - 1) (-.beta) (-.alpha)
                    in
                    let best_score, best_pos =
                      if score > best_score then (score, pos) else (best_score, best_pos)
                    in
                    let alpha = max alpha score in
                    if alpha >= beta then (best_score, Some best_pos)
                    else search_moves best_score best_pos alpha rest
              in
              search_moves (-.infty) (List.hd moves) alpha moves
      in
      store table board color depth alpha_original beta_original score best_move;
      score

(* 指定深さだけ探索する。根局面もnegamaxに通すことで、置換表に
   根の最善手を保存し、次の反復で手順序として利用できるようにする。 *)
let search_at_depth ~eval ~table ~deadline ~nodes board color depth =
  Time_control.check_now deadline;
  let score =
    negamax ~eval ~table ~deadline ~nodes board color depth (-.infty) infty
  in
  match cached_entry table board color with
  | Some { best_move = Some pos; _ } -> (Mv pos, score)
  | _ -> (Pass, score)

(* 深さ1からdepthまで順に探索する反復深化。
   浅い反復で得た最善手が深い反復のαβ探索における優先手になる。 *)
let best_move ~eval ~depth ?time_ms board color =
  let table = Transposition_table.create 100_003 in
  let deadline =
    Option.map (fun ms -> Time_control.deadline_of ms (empty_count board)) time_ms
  in
  let nodes = ref 0 in
  let max_depth = max 1 depth in
  let fallback =
    match order_moves board color (valid_moves board color) with
    | pos :: _ -> (Mv pos, -.infty)
    | [] -> (Pass, -.infty)
  in
  let rec deepen current_depth latest =
    if current_depth > max_depth then latest
    else
      try
        let result =
          search_at_depth ~eval ~table ~deadline ~nodes board color current_depth
        in
        deepen (current_depth + 1) result
      with Time_control.Timeout -> latest
  in
  deepen 1 fallback
