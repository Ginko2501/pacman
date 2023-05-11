type state = Up | Down | Left | Right | Dead

type t = {
  mutable x : int;
  mutable y : int;
  mutable s : state
}

val init : int -> int -> t

val get_x : t -> int

val get_y : t -> int

val get_state : t -> state

val set_x : t -> int -> unit

val set_y : t -> int -> unit

val set_state : t -> state -> unit

val draw_pacman: (int * int) -> int -> int -> unit