open Command

(** ビット配置（j = 1 が盤面の上端）:
    0 1 2 3 4 5 6 7
    ...
    56 57 58 59 60 61 62 63
    (MSB: 63, LSB:0)
    *)
type board = {
  black : Int64.t;
  white : Int64.t;
}

let initial_black = Int64.logor (Int64.shift_left 1L 28) (Int64.shift_left 1L 35)
let initial_white = Int64.logor (Int64.shift_left 1L 27) (Int64.shift_left 1L 36)

let init_board () = { black = initial_black; white = initial_white }

let dirs = [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

let file_a = 0x0101010101010101L
let file_h = 0x8080808080808080L
let not_file_a = Int64.lognot file_a
let not_file_h = Int64.lognot file_h

(* 図で見た方向に合わせる: north は上、east は右。 *)
let shift_n x = Int64.shift_right_logical x 8
let shift_s x = Int64.shift_left x 8
let shift_e x = Int64.logand (Int64.shift_left x 1) not_file_a
let shift_w x = Int64.logand (Int64.shift_right_logical x 1) not_file_h
let shift_ne x = Int64.logand (Int64.shift_right_logical x 7) not_file_a
let shift_nw x = Int64.logand (Int64.shift_right_logical x 9) not_file_h
let shift_se x = Int64.logand (Int64.shift_left x 9) not_file_a
let shift_sw x = Int64.logand (Int64.shift_left x 7) not_file_h

let shift_for_direction = function
  | (-1, -1) -> shift_nw
  | (0, -1) -> shift_n
  | (1, -1) -> shift_ne
  | (-1, 0) -> shift_w
  | (1, 0) -> shift_e
  | (-1, 1) -> shift_sw
  | (0, 1) -> shift_s
  | (1, 1) -> shift_se
  | _ -> invalid_arg "shift_for_direction: non-adjacent direction"

let bit_of_pos (i, j) =
  if i < 1 || i > 8 || j < 1 || j > 8 then
    invalid_arg "bit_of_pos: position outside the board"
  else Int64.shift_left 1L (((j - 1) * 8) + (i - 1))

let pos_of_index k = ((k mod 8) + 1, (k / 8) + 1)

let bits_to_positions bits =
  let rec loop k result =
    if k = 64 then List.rev result
    else
      let result =
        if Int64.logand bits (Int64.shift_left 1L k) <> 0L then
          pos_of_index k :: result
        else result
      in
      loop (k + 1) result
  in
  loop 0 []

let stones board color = if color = Color.black then board.black else board.white

let empty_squares board = Int64.lognot (Int64.logor board.black board.white)

let legal_moves_mask board color =
  let mine = stones board color in
  let opponent = stones board (Color.opposite color) in
  let empty = empty_squares board in
  let moves_in_direction shift =
    let x = ref (Int64.logand (shift mine) opponent) in
    for _ = 1 to 5 do
      x := Int64.logor !x (Int64.logand (shift !x) opponent)
    done;
    Int64.logand (shift !x) empty
  in
  List.fold_left
    (fun moves direction -> Int64.logor moves (moves_in_direction (shift_for_direction direction)))
    0L dirs

let flips_in_direction mine opponent move_bit shift =
  let cursor = ref (shift move_bit) in
  let flips = ref 0L in
  for _ = 1 to 6 do
    if Int64.logand !cursor opponent <> 0L then (
      flips := Int64.logor !flips !cursor;
      cursor := shift !cursor)
  done;
  if Int64.logand !cursor mine <> 0L then !flips else 0L

let flips_mask board color pos =
  let move_bit = bit_of_pos pos in
  let mine = stones board color in
  let opponent = stones board (Color.opposite color) in
  List.fold_left
    (fun flips direction ->
      Int64.logor flips
        (flips_in_direction mine opponent move_bit (shift_for_direction direction)))
    0L dirs

let flippable_indices_line board color direction pos =
  let move_bit = bit_of_pos pos in
  let mine = stones board color in
  let opponent = stones board (Color.opposite color) in
  bits_to_positions
    (flips_in_direction mine opponent move_bit (shift_for_direction direction))

let flippable_indices board color pos = bits_to_positions (flips_mask board color pos)
let is_effective board color pos = flips_mask board color pos <> 0L

let is_valid_move board color pos =
  let bit = bit_of_pos pos in
  Int64.logand bit (empty_squares board) <> 0L && is_effective board color pos

let do_move board command color =
  match command with
  | GiveUp | Pass -> board
  | Mv pos ->
      if not (is_valid_move board color pos) then invalid_arg "do_move: invalid move";
      let move_bit = bit_of_pos pos in
      let flips = flips_mask board color pos in
      if color = Color.black then
        {
          black = Int64.logor board.black (Int64.logor move_bit flips);
          white = Int64.logand board.white (Int64.lognot flips);
        }
      else
        {
          black = Int64.logand board.black (Int64.lognot flips);
          white = Int64.logor board.white (Int64.logor move_bit flips);
        }

let valid_moves board color = bits_to_positions (legal_moves_mask board color)

let popcount bits =
  let rec loop remaining count =
    if remaining = 0L then count
    else loop (Int64.logand remaining (Int64.sub remaining 1L)) (count + 1)
  in
  loop bits 0

let count board color = popcount (stones board color)

(* 相手の石に隣接する空きマスを数える。 *)
let potential_mobility board color =
  let opponent = stones board (Color.opposite color) in
  let adjacent_to_opponent =
    List.fold_left
      (fun adjacent direction ->
        Int64.logor adjacent (shift_for_direction direction opponent))
      0L dirs
  in
  popcount (Int64.logand adjacent_to_opponent (empty_squares board))

let empty_count board = 64 - count board Color.black - count board Color.white

let color_at board pos =
  let bit = bit_of_pos pos in
  if Int64.logand board.black bit <> 0L then Color.black
  else if Int64.logand board.white bit <> 0L then Color.white
  else Color.none

let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j = 1 to 8 do
    print_int j;
    print_string "|";
    for i = 1 to 8 do
      Color.print (color_at board (i, j));
      print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"

let report_result board =
  let black_count = count board Color.black in
  let white_count = count board Color.white in
  print_endline "========== Final Result ==========";
  if black_count > white_count then print_endline "*Black wins!*"
  else if black_count < white_count then print_endline "*White wins!*"
  else print_endline "*Even*";
  print_endline ("Black: " ^ string_of_int black_count);
  print_endline ("White: " ^ string_of_int white_count);
  print_board board
