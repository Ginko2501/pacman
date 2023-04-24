open Graphics

type state = Player | Bot | Wall | Empty | Dot

type t = {
  x : int;
  y : int;
  mutable s : state
}

let make (x : int) (y : int) (s : state) : t = {x; y; s}

(* [plot v] plots the voxel v onto the screen *)
let plot (v : t) =
  match v.s with
  | _ -> 
      set_color yellow; 
      fill_circle v.x v.y 20;
      set_color black;
      fill_arc v.x v.y 20 20 (-27) 27;