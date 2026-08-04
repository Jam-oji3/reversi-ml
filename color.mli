(** 色（マスの状態）のためのモジュール *)

(** 色 *)
type t

(** 空きマス *)
val none : t

val white : t
val black : t

(** 番兵 *)
val sentinel : t

val opposite : t -> t
val string : t -> string
val print : t -> unit
