include module type of Bitboard

(** 次の手を選択するメイン関数 *)
val play : ?time_ms:int -> board -> Color.t -> Command.move
