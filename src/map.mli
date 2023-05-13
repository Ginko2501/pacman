(** Map. *)

(** Draw the main map and control the movement of pacman and bot. *)

type t = {
  mutable voxels : Voxel.t list;
  mutable ended : bool;
}
(** [t] is the map as a list of voxels *)

val init : unit -> t
(** [init_helper] is a helper function for [init] that creates a list of voxels
    that represents the map *)

val reset : t -> unit
(** [reset] resets the map to the initial state. *)

val get_voxel : t -> int -> int -> Voxel.t option
(** [get_voxel] returns the voxel at the given coordinates *)

val get_voxel_state : t -> int -> int -> Voxel.state option
(** [get_voxel_state] returns the type of the voxel at the given coordinates *)

val set_voxel : t -> int -> int -> Voxel.t -> unit
(** [set_voxel] sets the voxel at the given coordinates to the given voxel *)

val set_voxel_state : t -> int -> int -> Voxel.state -> unit
(** [set_voxel_state] sets the voxel at given coordinates to the given state *)

val move_player : t -> Player.t -> unit
(** [move_player] moves the player as its direction shows. *)

val move_bot : t -> Bot.t -> unit
(** [move_player] moves the bot as its direction shows. *)

val get_player : t -> Voxel.t option
(** [get_player] returns the player voxel *)

val get_player_state : t -> Voxel.state option
(** [get_player_state] returns the state of the player voxel *)

val set_player : t -> Voxel.t -> unit
(** [set_player] sets the player voxel to the given voxel *)

val set_player_state : t -> Voxel.state -> unit
(** [set_player_state] sets the player voxel to the given state *)

val draw : t -> Player.t -> Bot.t -> Bot.t -> unit
(** [draw m] draws the map [m] *)
