type wl = Win | Lose | Tie

type move = Mv of Pos.t | Pass | GiveUp

val string_of_move : move -> string

(** サーバ、クライアント間の通信コマンド *)
type command =
    Open of string (** player name *)
  | End of wl * int * int * string  (** result, your stones, opponent's stones, reason *)
  | Move of move
  | Start of Color.t * string * int (** color, oppnent's name, assinged time (in ms) *)
  | Ack of int (** updated assigned time (in ms) *)
  | Bye of (string * (int * int * int)) list
  | Empty

val string_of_command_type : command -> string


