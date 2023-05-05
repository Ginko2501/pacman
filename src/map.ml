let rec map_ver n x y acc = 
  if n = y then acc else map_ver (n) (x) (y + 1) ((x, y) :: acc)
let rec map_ho n x y acc = 
  if n = x then acc else map_ho (n) (x+1) (y) ((x, y) :: acc)

let map h n x y = if h then map_ho (x + n) x y [] else map_ver (y + n) x y []

let wall = (map false 10 0 0) @ (map true 10 0 0) @ (map true 2 2 2) @ (map false 3 2 2)
let wall2 = (map false 2 5 1) @ (map true 3 4 4)
let m = wall2 @ (List.map (fun (x, y)->(x, 10-y)) wall2)
let n = (wall @ (List.map (fun (x, y)->(x, 10-y)) wall) @ (List.map (fun (x, y)->(10- x, 10-y)) wall)) @ (List.map (fun (x, y)->(10- x, y)) wall)

let map_wall = List.map (fun (x, y) -> (x * 40 + 20, y * 40 + 20)) (m @ n)