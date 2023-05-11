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
  x : int;
  y : int;
  mutable s : state
}

(* [init_pacman x y] is the initial pacman with coordinates [x] and [y]. *)
let init_pacman x y = {
  x = x;
  y = y;
  s = Right
}

(* [get_x p] is the x coordinate of the pacman [p]. *)
let get_x p = p.x

(* [get_y p] is the y coordinate of the pacman [p]. *)
let get_y p = p.y

(* [get_state p] is the state of the pacman [p]. *)
let get_state p = p.s

(* [set_state p s] sets the state of the pacman [p] to [s]. *)
let set_state p s = p.s <- s

(* [move_up p] moves the pacman [p] up. *)


(* [draw_pacman (x, y) a1 a2] draws the pacman with center at [(x, y)] and
   angle [a1] to [a2]. *)


let draw_pacman (x, y) a1 a2 =
  set_color yellow;
  fill_circle x y 16;
  (* draw with background color*)
  set_color black;
  fill_arc x y 16 16 a1 a2

(* let self_empty map p = Player.(Map.set_voxel_state map p.x p.y Empty)

let move_pacman map p = 
  let key = read_key () in
  if key = 'd' then 
    (print_endline "right";
    (Player.set_state p Right;
    (if Map.get_voxel_state map (p.x+1) p.y = Some Wall then map
    else (Map.set_voxel_state (self_empty map p) (p.x+1) p.y Player))))
  else if key = 'a' then 
    (Player.set_state p Left;
    (if Map.get_voxel_state map (p.x-1) p.y = Some Wall then map
    else (Map.set_voxel_state (self_empty map p) (p.x-1) p.y Player)))
  else if key = 'w' then 
    (Player.set_state p Up;
    (if Map.get_voxel_state map p.x (p.y+1) = Some Wall then map
    else (Map.set_voxel_state (self_empty map p) p.x (p.y+1) Player)))
  else if key = 's' then 
    (Player.set_state p Down;
    (if Map.get_voxel_state map p.x (p.y-1) = Some Wall then map
    else (Map.set_voxel_state (self_empty map p) p.x (p.y+1) Player)))
  else map *)
