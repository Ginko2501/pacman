(** Player. *)

(** Player defines and draws the pacman. *)

(** [state] is the direction of the pacman. *)
type state =
  | Up
  | Down
  | Left
  | Right
  | Dead

type t = {
  mutable x : int;
  mutable y : int;
  mutable s : state;
  mutable score : int;
}
(** [t] is the coordinates, state, and score of the pacman. *)

val init : int -> int -> t
(** [init] creates a new pacman. *)

val get_x : t -> int
(** [get_x] returns the x coordinate of the pacman. *)

val get_y : t -> int
(** [get_y] returns the y coordinate of the pacman. *)

val get_xy : t -> int * int
(** [get_xy] returns the x and y coordinates of the pacman. *)

val get_state : t -> state
(** [get_state] returns the state of the pacman. *)

val get_score : t -> int
(** [get_score] returns the score of the pacman. *)

val set_x : t -> int -> unit
(** [set_x] sets the x coordinate of the pacman. *)

val set_y : t -> int -> unit
(** [set_y] sets the y coordinate of the pacman. *)

val set_state : t -> state -> unit
(** [set_state] sets the state of the pacman. *)

val set_score : t -> int -> unit
(** [set_score] sets the score of the pacman. *)

val inc_score : t -> unit
(** [inc_score] increases the score of the pacman by 1. *)

val dec_score : t -> unit
(** [dec_score] decreases the score of the pacman by 1. *)

val reset_score : t -> unit
(** [reset_score] resets the score of the pacman to 0. *)

val draw_pacman : t -> unit
(** [draw_pacman] draws the pacman. *)
