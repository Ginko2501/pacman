(** Bot *)

(** Bot defines and draws the two ghosts that chase the pacman. *)

(** [state] of bot is the direction of it moving. *)
type state =
  | Up
  | Down
  | Left
  | Right

(** [ghost_type] is one of the two ghosts. *)
type ghost_type =
  | Ghost1
  | Ghost2

type t = {
  mutable x : int;
  mutable y : int;
  mutable s : state;
  g : ghost_type;
}
(** [t] is the ghost with coordinates, state, and ghost type. *)

val init_ghost : int -> int -> ghost_type -> t
(** [init_ghost] creates a new ghost. *)

val get_x : t -> int
(** [get_x] returns the x coordinate of the ghost. *)

val get_y : t -> int
(** [get_y] returns the y coordinate of the ghost. *)

val get_xy : t -> int * int
(** [get_xy] returns the x and y coordinates of the ghost. *)

val set_xy : t -> int * int -> unit
(** [set_xy] sets the x and y coordinates of the ghost. *)

val get_ghost_type : t -> ghost_type
(** [get_ghost_type] returns the ghost type of the ghost. *)

val get_state : t -> state
(** [get_state] returns the state of the ghost. *)

val set_state : t -> state -> unit
(** [set_state] sets the state of the ghost. *)

val get_ghost : t -> ghost_type
(** [get_ghost] returns the ghost type of the ghost. *)

val draw_ghost : t -> unit
(** [draw_ghost] draws the ghost. *)

val set_x : t -> int -> unit
(** [set_x] sets the x coordinate of the ghost. *)

val set_y : t -> int -> unit
(** [set_x] sets the x coordinate of the ghost. *)
