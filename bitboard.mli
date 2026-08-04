(** Bitboard: A1が最下位ビット *)
type board

val init_board : unit -> board
val dirs : (int * int) list
val flippable_indices_line : board -> Color.t -> int * int -> Pos.t -> Pos.t list
val flippable_indices : board -> Color.t -> Pos.t -> Pos.t list
val is_effective : board -> Color.t -> Pos.t -> bool
val legal_moves_mask : board -> Color.t -> Int64.t
val is_valid_move : board -> Color.t -> Pos.t -> bool
val do_move : board -> Command.move -> Color.t -> board
val valid_moves : board -> Color.t -> Pos.t list
val count : board -> Color.t -> int
val potential_mobility : board -> Color.t -> int
val print_board : board -> unit
val report_result : board -> unit
val empty_count : board -> int
val color_at : board -> Pos.t -> Color.t
