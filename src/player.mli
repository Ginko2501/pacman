type state = Up | Down | Left | Right | Dead

type t = {
  mutable x : int;
  mutable y : int;
  mutable s : state;
  mutable score : int
}

val init : int -> int -> t

val get_x : t -> int

val get_y : t -> int

val get_state : t -> state

val get_score : t -> int

val set_x : t -> int -> unit

val set_y : t -> int -> unit

val set_state : t -> state -> unit

val inc_score : t -> unit

val draw_pacman: t -> unit