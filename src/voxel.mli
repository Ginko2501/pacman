(** Voxel. *)

(** Voxel defines and draws voxels. It makes the map into a grid system so it's
    easier to draw. *)

(** [state] is the state of a voxel [Player] is the player state [Bot] is the
    bot state [Wall] is the wall state [Empty] is the empty state [Dot] is the
    dot state *)
type state =
  | Player
  | Bot
  | Wall
  | Empty
  | Dot

type t = {
  x : int;
  y : int;
  mutable s : state;
}
(** [t] is the type of a voxel [x] is the x coordinate of the voxel [y] is the y
    coordinate of the voxel [s] is the state of the voxel *)

val make : state -> int * int -> t
(** [make s x y] is a voxel with state [s] and coordinates [x] and [y] *)

val get_state : t -> state
(** [get_state v] is the state of [v] *)

val set_state : t -> state -> unit
(** [set_state v s] sets the state of [v] to [s] *)

val get_x : t -> int
(** [get_x v] is the x coordinate of [v] *)

val get_y : t -> int
(** [get_y v] is the y coordinate of [v] *)

val get_coords : t -> int * int
(** [get_coords] is the coordinates of [v] *)

val string_of_state : state -> string
(** [string_of_state s] is the string representation of [s] *)

val draw : t -> unit
(** [plot v] plots [v] on the graphics window. If the state of v is [Player],
    then a pacman is drawn. If the state of v is [Dot], then a white circle is
    drawn. If the state of v is [Wall], then a blue square is drawn. If the
    state of v is [Empty], then a black square is drawn. If the state of v is
    [Bot], then a red circle is drawn. *)
