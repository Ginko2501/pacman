open Graphics
type status = Up | Down | Left | Right | Dead

type t = {
  x : int;
  y : int;
  mutable s : status
}

let draw_pacman (x, y) a1 a2 =
  set_color yellow;
  fill_circle x y 16;
  (* draw with background color*)
  set_color black;
  fill_arc x y 16 16 a1 a2
