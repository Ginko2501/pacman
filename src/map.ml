open Voxel

(* [height] is the number of voxels in the y direction
   [width] is the number of voxels in the x direction *)
let height = 20
let width = 20

(* [t] is the map as a list of voxels *)
type t = {
  voxels : Voxel.t list;
}

(* [init_helper] is a helper function for [init] that creates a list of voxels
   that represents the map *)
let rec init_helper 
  (x : int) 
  (y : int) 
  (voxels : Voxel.t list)
: Voxel.t list =
  if x = width then 
    if y = height - 1 then 
      voxels
    else 
      init_helper 0 (y+1) voxels
  else 
    if x = 0 || x = width-1 || y = 0 || y = height-1 then 
      init_helper (x+1) y (Voxel.make Wall (40*x, 40*y) :: voxels)
    else 
      init_helper (x+1) y (Voxel.make Player (40*x, 40*y) :: voxels)

(* [init] returns the initial map *)
let init () : t = 
  let voxels = init_helper 0 0 [] in
  { voxels = voxels }

(* [get_voxel] returns the voxel at the given coordinates *)
let rec get_voxel (m : t) (x : int) (y : int) : Voxel.t option =
  match m.voxels with
  | [] -> None
  | h :: t -> 
    if Voxel.get_x h = x && Voxel.get_y h = y then 
      Some h
    else 
      get_voxel { voxels = t } x y

(* [get_voxel_state] returns the type of the voxel at the given coordinates *)
let get_voxel_state (m : t) (x : int) (y : int) : Voxel.state option =
  match get_voxel m x y with
  | None -> None
  | Some v -> Some v.s

(* [set_voxel] sets the voxel at the given coordinates to the given voxel *)
(* make this t->int->int->Voxel.t->unit *)
let set_voxel (m : t) (x : int) (y : int) (v : Voxel.t) : unit =
  match get_voxel m x y with
  | None -> ()
  | Some voxel -> voxel.s <- v.s

(* [set_voxel_state] sets the voxel at given coordinates to the given state *)
let set_voxel_state (m : t) (x : int) (y : int) (s : Voxel.state) : unit =
  match get_voxel m x y with
  | None -> ()
  | Some v -> Voxel.set_state v s

(* [get_player] returns the player voxel *)
(* let rec get_player (m : t) : Voxel.t option = 
  match m.voxels with
  | [] -> None
  | h :: t -> 
    if Voxel.get_state h = Player then Some h
    else get_player { voxels = t } *)

(* [get_player_state] returns the state of the player voxel *)
(* let get_player_state (m : t) : Voxel.state option =
  match get_player m with
  | None -> None
  | Some p -> Some p.s *)

(* [set_player] sets the player voxel to the given voxel *)
(* let set_player (m : t) (v : Voxel.t) : unit =
  match get_player m with
  | None -> ()
  | Some p -> 
      Voxel.set_state p Empty;
      set_voxel m (Voxel.get_x v) (Voxel.get_y v) v *)

(* [set_player_state] sets the player voxel to the given state *)
(* let set_player_state (m : t) (s : Voxel.state) : unit =
  match get_player m with
  | None -> ()
  | Some p -> 
      Voxel.set_state p s;
      set_voxel_state m (Voxel.get_x p) (Voxel.get_y p) s *)


      
(* [draw] draws the map *)
let rec draw (m : t) = 
  match m.voxels with
  | [] -> ()
  | h :: t -> 
    Voxel.draw h;
    draw { voxels = t }