type t = {
  voxels : Voxel.t list;
}

val init : unit -> t

val draw : t -> unit
