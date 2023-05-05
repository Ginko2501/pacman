open Graphics
type state = Player | Bot | Wall | Empty | Dot

type t = {
  x : int;
  y : int;
  mutable s : state 
}

let make (s : state) (x, y) : t = {x; y; s}

(* [plot v] plots the voxel v onto the screen *)
let plot (v : t) =
  match v.s with
  | Player -> Pacman.draw_pacman (v.x, v.y) (-27) 27
  | Dot -> set_color white; 
          fill_circle v.x v.y 5
  | Wall -> set_color blue; 
            draw_rect (v.x-20) (v.y-20) 40 40;
  | Empty -> set_color black; 
            fill_rect (v.x-10) (v.y-10) 20 20
  | Bot -> set_color red;
          fill_circle v.x v.y 8;
          fill_rect (v.x - 8) (v.y - 10) 16 10
