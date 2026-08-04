open Command
open Bitboard

let choose_move ?time_ms board color =
  match valid_moves board color with
  | [] -> Pass
  | _ ->
      if Solve.can_solve board then
        let deadline =
          Option.map
            (fun ms -> Time_control.deadline_of ms (empty_count board))
            time_ms
        in
        (try fst (Solve.best_move ?deadline board color)
         with Time_control.Timeout -> Mv (List.hd (valid_moves board color)))
      else
        fst (Search.best_move ~eval:Eval.eval_board ~depth:12 ?time_ms board color)
