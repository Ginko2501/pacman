open Voxel

let height = 20

let width = 20

type t = {
  voxels : Voxel.t list;
}

(* let rec init_walls 
  (x : int) 
  (y : int) 
  (walls : (int * int) list)
: (int * int) list =  *)


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

let init () : t = 
  let voxels = init_helper 0 0 [] in
  { voxels = voxels }

(* let init () : t = 
  let voxels = [] in 
  for i = 0 to height-1 do
    for j = 0 to width-1 do
      ignore (voxels = Voxel.make Player (i, j) :: voxels);
    done;
  done;
  print_endline ("?"^(string_of_int (List.length voxels)));
  (* ignore(
    if voxels.(0).s = Player then 
      print_endline "Player"
    else 
      print_endline "Not Player"
  ); *)
  { voxels = voxels} *)

let rec draw (m : t) = 
  match m.voxels with
  | [] -> ()
  | h :: t -> 
    Voxel.draw h;
    draw { voxels = t }