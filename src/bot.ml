open Graphics

(* [state] is the state of the pacman. [Up] is the state when the pacman is
   moving up. [Down] is the state when the pacman is moving down. [Left] is the
   state when the pacman is moving left. [Right] is the state when the pacman is
   moving right. [Dead] is the state when the pacman is dead. *)
type state =
  | Up
  | Down
  | Left
  | Right

type ghost_type =
  | Ghost1
  | Ghost2

(* [t] is the type of the pacman. [x] is the x coordinate of the pacman. [y] is
   the y coordinate of the pacman. [s] is the state of the pacman. *)
type t = {
  mutable x : int;
  mutable y : int;
  mutable s : state;
  g : ghost_type;
}

(* [init_ghost x y] is the initial ghost with coordinates [x] and [y]. *)
let init_ghost x y gtype = { x; y; s = Right; g = gtype }

(* [get_x p] is the x coordinate of the ghost [p]. *)
let get_x p = p.x

(* [get_y p] is the y coordinate of the ghost [p]. *)
let get_y p = p.y

let get_xy p = (p.x, p.y)

let get_ghost_type p = p.g

(* [get_state p] is the state of the ghost [p]. *)
let get_state p = p.s

(* [set_state p s] sets the state of the ghost [p] to [s]. *)
let set_state p s = p.s <- s
let get_ghost p = p.g

(* [set_x p x] sets the x coordinate of the ghost [p] to [x]. *)
let set_x p x = p.x <- x

(* [set_y p y] sets the y coordinate of the ghost [p] to [y]. *)
let set_y p y = p.y <- y

let set_xy p (x, y) =
  p.x <- x;
  p.y <- y


let draw_ghost g =
  if get_ghost g = Ghost1 then set_color red else set_color blue;
  let r = 16 in
  fill_arc g.x g.y r r 0 180;
  fill_poly
    [|
      (g.x - r, g.y);
      (g.x - r, g.y - r);
      (g.x - r + 3, g.y - (r / 2));
      (g.x - r + 6, g.y - r);
      (g.x - r + 9, g.y - (r / 2));
      (g.x - r + 12, g.y - r);
      (g.x - r + 15, g.y - (r / 2));
      (g.x - r + 18, g.y - r);
      (g.x - r + 21, g.y - (r / 2));
      (g.x - r + 24, g.y - r);
      (g.x - r + 27, g.y - (r / 2));
      (g.x - r + 30, g.y - r);
      (g.x + r, g.y - r);
      (g.x + r, g.y);
    |];
  set_color blue;
  fill_circle (g.x - 8) (g.y + 2) 4;
  fill_circle (g.x + 8) (g.y + 2) 4
