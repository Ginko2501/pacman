open Graphics

(*  [state] is the state of the pacman.
    [Up] is the state when the pacman is moving up.
    [Down] is the state when the pacman is moving down.
    [Left] is the state when the pacman is moving left.
    [Right] is the state when the pacman is moving right.
    [Dead] is the state when the pacman is dead. *)
type state = Up | Down | Left | Right | Dead

(* [t] is the type of the pacman.
   [x] is the x coordinate of the pacman.
   [y] is the y coordinate of the pacman.
   [s] is the state of the pacman. *)
type t = {
  mutable x : int;
  mutable y : int;
  mutable s : state;
  mutable score : int
}

(* [init_pacman x y] is the initial pacman with coordinates [x] and [y]. *)
let init x y = {
  x = x;
  y = y;
  s = Right;
  score = 0
}

(* [get_x p] is the x coordinate of the pacman [p]. *)
let get_x p = p.x

(* [get_y p] is the y coordinate of the pacman [p]. *)
let get_y p = p.y

(* [get_state p] is the state of the pacman [p]. *)
let get_state p = p.s

(* [get_score p] is the score of the pacman [p]. *)
let get_score p = p.score

(* [set_x p x] sets the x coordinate of the pacman [p] to [x]. *)
let set_x p x = p.x <- x

(* [set_y p y] sets the y coordinate of the pacman [p] to [y]. *)
let set_y p y = p.y <- y

(* [set_state p s] sets the state of the pacman [p] to [s]. *)
let set_state p s = p.s <- s

(* [inc_score p] increments the score of the pacman [p] by 1. *)
let inc_score p = p.score <- p.score + 1

(* [draw_pacman (x, y) a1 a2] draws the pacman with center at [(x, y)] and
   angle [a1] to [a2]. *)


let draw_pacman p =
  let x = p.x in
  let y = p.y in 
  set_color yellow;
  fill_circle x y 16;
  (* draw with background color*)
  set_color black;
  if get_state p = Right then 
  fill_arc x y 16 16 (-25) 25
  else if get_state p = Left then
  fill_arc x y 16 16 (155) 206
  else if get_state p = Up then
  fill_arc x y 16 16 (65) 115 
  else if get_state p = Down then
  fill_arc x y 16 16 (245) 296

