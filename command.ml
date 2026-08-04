type wl   = Win | Lose | Tie
type move = Mv of Pos.t | Pass | GiveUp

let string_of_move = function
  | Pass   -> "PASS"
  | GiveUp -> "GIVEUP"
  | Mv pos -> Pos.string pos


type command =
  | Open of string (* player name *)
  | End of wl * int * int * string
      (* result, your stones, opponent's stones, reason *)
  | Move of move
  | Start of Color.t * string * int
      (* color, oppnent's name, assinged time (in ms) *)
  | Ack of int (* updated assigned time (in ms) *)
  | Bye of (string * (int * int * int)) list
  | Empty

let string_of_command_type = function
  | Open _ -> "Open"
  | End _ -> "End"
  | Move _ -> "Move"
  | Start _ -> "Start"
  | Ack _ -> "Ack"
  | Bye _ -> "Bye"
  | Empty -> "Empty"
