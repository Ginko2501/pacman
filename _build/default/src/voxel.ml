open Graphics

(* [state] is the state of a voxel 
   [Player] is the player state
   [Bot] is the bot state
   [Wall] is the wall state
   [Empty] is the empty state
   [Dot] is the dot state *)
type state = Player | Bot | Wall | Empty | Dot

(* [t] is the type of a voxel
   [x] is the x coordinate of the voxel
   [y] is the y coordinate of the voxel
   [s] is the state of the voxel *)
type t = {
  x : int;
  y : int;
  mutable s : state 
}

(* [make s x y] is a voxel with state [s] and coordinates [x] and [y] *)
let make (s : state) (x, y) : t = {x; y; s}

(* [get_state v] is the state of [v] *)
let get_state (v : t) : state = v.s

(* [set_state v s] sets the state of [v] to [s] *)
let set_state (v : t) (s : state) : unit = v.s <- s

(* [get_x v] is the x coordinate of [v] *)
let get_x (v : t) : int = v.x

(* [get_y v] is the y coordinate of [v] *)
let get_y (v : t) : int = v.y

(* [get_coords] is the coordinates of [v] *)
let get_coords (v : t) : int * int = (v.x, v.y)

(* [string_of_state s] is the string representation of [s] *)
let string_of_state = function
  | Player -> "Player"
  | Bot -> "Bot"
  | Wall -> "Wall"
  | Empty -> "Empty"
  | Dot -> "Dot"

(* [plot v] plots [v] on the graphics window.
   If the state of v is [Player], then a pacman is drawn.
   If the state of v is [Dot], then a white circle is drawn.
   If the state of v is [Wall], then a blue square is drawn.
   If the state of v is [Empty], then a black square is drawn.
   If the state of v is [Bot], then a red circle is drawn. *)
   let draw (v : t) =
    match v.s with
    | Player -> 
        set_color black;
        fill_rect (v.x-20) (v.y-20) 40 40;
        Player.draw_pacman (v.x, v.y) (-25) 25
    | Dot -> 
        set_color black;
        fill_rect (v.x-20) (v.y-20) 40 40;
        set_color yellow; 
        fill_circle v.x v.y 5
    | Wall -> 
        set_color black;
        fill_rect (v.x-20) (v.y-20) 40 40;
        set_color blue; 
        fill_rect (v.x-18) (v.y-18) 36 36;
    | Empty -> 
        set_color black; 
        fill_rect (v.x-20) (v.y-20) 40 40
    | Bot -> 
      set_color black;
      fill_rect (v.x-20) (v.y-20) 40 40;
      set_color red;
      let r = 16 in
      fill_arc v.x v.y r r 0 180;
      fill_poly [|(v.x-r,   v.y);       (v.x-r,    v.y-r);
                  (v.x-r+3, v.y-r/2);   (v.x-r+6,  v.y-r);
                  (v.x-r+9, v.y-r/2);   (v.x-r+12, v.y-r);
                  (v.x-r+15, v.y-r/2);   (v.x-r+18, v.y-r);
                  (v.x-r+21, v.y-r/2);   (v.x-r+24, v.y-r);
                  (v.x-r+27, v.y-r/2);   (v.x-r+30, v.y-r);
                  (v.x+r,   v.y-r);     (v.x+r,    v.y)  ;|];
      set_color black;
      fill_circle (v.x - 8) (v.y + 2) 4;
      fill_circle (v.x + 8) (v.y + 2) 4;



