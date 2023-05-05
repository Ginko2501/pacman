type state = Player | Bot | Wall | Empty | Dot

type t = {
  x : int;
  y : int;
  mutable s : state
}

val make : state -> int * int -> t
(**makes a new voxel with the given x and y coordinates and state*)

val plot : t -> unit
(**plots the voxel v onto the screen *)