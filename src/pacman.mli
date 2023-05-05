type status = Up | Down | Left | Right | Dead

type t = {
  x : int;
  y : int;
  mutable s : status
}

val draw_pacman: (int * int) -> int -> int -> unit