(* [t] is the map as a list of voxels *)
type t = {
  voxels : Voxel.t list;
}

(* [init_helper] is a helper function for [init] that creates a list of voxels
   that represents the map *)
val init : unit -> t

(* [get_voxel] returns the voxel at the given coordinates *)
val get_voxel : t -> int -> int -> Voxel.t option

(* [get_voxel_state] returns the type of the voxel at the given coordinates *)
val get_voxel_state : t -> int -> int -> Voxel.state option

(* [set_voxel] sets the voxel at the given coordinates to the given voxel *)
val set_voxel : t -> int -> int -> Voxel.t -> unit

(* [set_voxel_state] sets the voxel at given coordinates to the given state *)
val set_voxel_state : t -> int -> int -> Voxel.state -> unit

val move_player : t -> Player.t -> unit

(* [get_player] returns the player voxel *)
(* val get_player : t -> Voxel.t option *)

(* [get_player_state] returns the state of the player voxel *)
(* val get_player_state : t -> Voxel.state option *)

(* [set_player] sets the player voxel to the given voxel *)
(* val set_player : t -> Voxel.t -> t *)

(* [set_player_state] sets the player voxel to the given state *)
(* val set_player_state : t -> Voxel.state -> t *)

(* [draw m] draws the map [m] *)
val draw : t -> unit
