exception Timeout

let endgame_start_empty = 13
let pre_endgame_start_empty = 16
let endgame_reserve_ms = 10_000
let safety_ms = 300
let min_search_ms = 5
let midgame_max_budget_ms = 1_200

(* 残り時間がごく短いときに、固定の safety_ms を引くと、下限の
   探索時間の方が残り時間を上回ってしまう。短時間では残り時間の10%
   を余裕として残し、必ず現在の持ち時間内に収める*)
let short_clock_budget time_ms requested_ms =
  if time_ms <= 0 then 25
  else
    let margin_ms = min safety_ms (max min_search_ms (time_ms / 10)) in
    let available_ms = max 0 (time_ms - margin_ms) in
    max min_search_ms (min requested_ms available_ms)

let budget_ms time_ms empty_squares =
  if time_ms <= 0 then 25
  else if empty_squares <= endgame_start_empty then
    (* 完全読みでは、予約しておいた時間を使う。 *)
    short_clock_budget time_ms time_ms
  else if empty_squares <= pre_endgame_start_empty then
    (* 空き14〜16では、完全読み用の時間を残して通常探索を深くする。 *)
    let surplus_ms = max 0 (time_ms - endgame_reserve_ms - safety_ms) in
    let max_budget_ms =
      if empty_squares <= 14 then 6_000
      else if empty_squares = 15 then 4_000
      else 3_000
    in
    min max_budget_ms surplus_ms
  else
    (* 完全読み開始時に 10 秒を残す。 *)
    let usable_ms = max 0 (time_ms - endgame_reserve_ms) in
    let estimated_turns =
      max 1 ((empty_squares - endgame_start_empty + 1) / 2)
    in
    min midgame_max_budget_ms (max 0 (usable_ms / estimated_turns - 50))

let deadline_of time_ms empty_squares =
  Unix.gettimeofday ()
  +. (float_of_int (budget_ms time_ms empty_squares) /. 1000.0)

let check_now = function
  | Some deadline when Unix.gettimeofday () >= deadline -> raise Timeout
  | _ -> ()

(* 各ノードの入口で確認する。 *)
let check deadline nodes =
  incr nodes;
  check_now deadline
