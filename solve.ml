open Command
open Bitboard
open Color

(* 終局間際の完全読み。 *)
let endgame_limit = 13
let infty = 1000

type bound = Exact | Lower | Upper

type entry = {
  score : int;
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

(* 終盤は、一度探索したらあとはその結果を使う *)
let persistent_table = Transposition_table.create 1_000_003
let max_cached_positions = 1_000_000

let table_for_search () =
  if Transposition_table.length persistent_table >= max_cached_positions then
    Transposition_table.clear persistent_table;
  persistent_table

let cached_entry table board color =
  Transposition_table.find_opt table (board, color)

(* 角・X/C の静的評価に、置換表の最善手を重ねて探索順を作る。 *)
let order_moves ?preferred board color moves =
  let square_score pos =
    Eval.score_square_type (Eval.classify_square pos board color)
  in
  let ordered =
    List.sort (fun pos1 pos2 -> compare (square_score pos2) (square_score pos1)) moves
  in
  match preferred with
  | None -> ordered
  | Some best_pos ->
      let best, rest = List.partition (fun pos -> pos = best_pos) ordered in
      best @ rest

type probe_result = Cut of int | Search of int * int * Pos.t option

let probe table board color alpha beta =
  match cached_entry table board color with
  | Some { score; bound; best_move } ->
      (match bound with
       | Exact -> Cut score
       | Lower ->
           let alpha = max alpha score in
           if alpha >= beta then Cut score else Search (alpha, beta, best_move)
       | Upper ->
           let beta = min beta score in
           if alpha >= beta then Cut score else Search (alpha, beta, best_move))
  | None -> Search (alpha, beta, None)

let store table board color alpha beta score best_move =
  let bound =
    if score <= alpha then Upper
    else if score >= beta then Lower
    else Exact
  in
  Transposition_table.replace table (board, color) { score; bound; best_move }

let final_score board color =
  let opp = opposite color in
  count board color - count board opp

let rec solve ?deadline ~nodes ~table board color alpha beta =
  Time_control.check deadline nodes;
  let alpha_original = alpha in
  let beta_original = beta in
  match probe table board color alpha beta with
  | Cut score -> score
  | Search (alpha, beta, preferred_move) ->
      let score, best_move =
        let moves =
          order_moves ?preferred:preferred_move board color (valid_moves board color)
        in
        match moves with
        | [] ->
            let opp = opposite color in
            if valid_moves board opp = [] then (final_score board color, None)
            else
              (-(solve ?deadline ~nodes ~table board opp (-beta) (-alpha)), None)
        | first :: rest ->
            let opp = opposite color in
            let score_of pos =
              -(solve ?deadline ~nodes ~table (do_move board (Mv pos) color) opp
                  (-beta) (-alpha))
            in
            let first_score = score_of first in
            let rec search_moves best_score best_pos alpha = function
              | [] -> (best_score, Some best_pos)
              | pos :: remaining ->
                  let score = score_of pos in
                  let best_score, best_pos =
                    if score > best_score then (score, pos) else (best_score, best_pos)
                  in
                  let alpha = max alpha score in
                  if alpha >= beta then (best_score, Some best_pos)
                  else search_moves best_score best_pos alpha remaining
            in
            let alpha = max alpha first_score in
            if alpha >= beta then (first_score, Some first)
            else search_moves first_score first alpha rest
      in
      store table board color alpha_original beta_original score best_move;
      score

let best_move ?deadline board color =
  let table = table_for_search () in
  let nodes = ref 0 in
  let moves =
    let preferred =
      match cached_entry table board color with
      | Some { best_move; _ } -> best_move
      | None -> None
    in
    order_moves ?preferred board color (valid_moves board color)
  in
  match moves with
  | [] -> (Pass, final_score board color)
  | fallback :: rest ->
      (* 時間切れでもその時点の最善手を返す *)
      let best_pos = ref fallback in
      let best_score = ref (-infty) in
      let alpha = ref (-infty) in
      let opp = opposite color in
      let search_one pos =
        let score =
          -(solve ?deadline ~nodes ~table (do_move board (Mv pos) color) opp
              (-infty) (- !alpha))
        in
        if score > !best_score then (
          best_score := score;
          best_pos := pos);
        alpha := max !alpha score
      in
      (try
         Time_control.check_now deadline;
         search_one fallback;
         List.iter search_one rest;
         store table board color (-infty) infty !best_score (Some !best_pos);
         (Mv !best_pos, !best_score)
       with Time_control.Timeout -> (Mv !best_pos, !best_score))

let can_solve board =
  empty_count board <= endgame_limit
