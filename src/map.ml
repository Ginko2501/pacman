open Voxel

(* [height] is the number of voxels in the y direction [width] is the number of
   voxels in the x direction *)
let height = 20
let width = 20

(* [t] is the map as a list of voxels *)
type t = {
  mutable voxels : Voxel.t list;
  mutable ended : bool;
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
      init_helper 0 (y + 1) voxels
  else if x = 0 || x = width - 1 || y = 0 || y = height - 1 then
    init_helper (x + 1) y
      (Voxel.make Wall ((40 * x) + 20, (40 * y) + 20) :: voxels)
  else if
    (x = 10 && (y = 1 || y = 2 || y = 3))
    || (x = 9 && (y = 16 || y = 18 || y = 17))
    || (x = 3 && (y = 16 || y = 15 || y = 14 || y = 13 || y = 12 || y = 11))
    || (x = 3 && (y = 8 || y = 7 || y = 5 || y = 4 || y = 6 || y = 3))
    || (x = 16 && (y = 16 || y = 15 || y = 14 || y = 13 || y = 12 || y = 11))
    || (x = 16 && (y = 8 || y = 7 || y = 5 || y = 4 || y = 6 || y = 3))
    || ((x = 4 || x = 5 || x = 6) && y = 16)
    || ((x = 4 || x = 5 || x = 6 || x = 7) && y = 3)
    || ((x = 15 || x = 14 || x = 13 || x = 12) && y = 16)
    || ((x = 15 || x = 14 || x = 13) && y = 3)
    || (x = 6 || x = 7 || x = 8 || x = 9 || x = 10 || x = 11 || x = 12 || x = 13)
       && (y = 6 || y = 13)
    || (x = 6 && (y = 7 || y = 8 || y = 9 || y = 10))
    || (x = 13 && (y = 11 || y = 12 || y = 9 || y = 10))
  then
    init_helper (x + 1) y
      (Voxel.make Wall ((40 * x) + 20, (40 * y) + 20) :: voxels)
  else if x = 1 && y = 1 then
    init_helper (x + 1) y
      (Voxel.make Player ((40 * x) + 20, (40 * y) + 20) :: voxels)
  else
    init_helper (x + 1) y
      (Voxel.make Dot ((40 * x) + 20, (40 * y) + 20) :: voxels)

(* [init] returns the initial map *)
let init () : t =
  let voxels = init_helper 0 0 [] in
  { voxels; ended = false }

let reset map = map.voxels <- init_helper 0 0 []

(* [get_voxel] returns the voxel at the given coordinates *)
let rec get_voxel 
  (m : t) 
  (x : int) 
  (y : int) 
: Voxel.t option =
  match m.voxels with
  | [] -> None
  | h :: t ->
      if Voxel.get_x h = x && Voxel.get_y h = y then 
        Some h
      else 
        get_voxel { 
          voxels = t; 
          ended = false 
        } x y

(* [get_voxel_state] returns the type of the voxel at the given coordinates *)
let get_voxel_state 
  (m : t) 
  (x : int) 
  (y : int) 
: Voxel.state option =
  match get_voxel m x y with
  | None -> 
      None
  | Some v -> 
      Some v.s

(* [set_voxel] sets the voxel at the given coordinates to the given voxel *)
(* make this t->int->int->Voxel.t->unit *)
let set_voxel 
  (m : t) 
  (x : int) 
  (y : int) 
  (v : Voxel.t) 
: unit =
  match get_voxel m x y with
  | None -> 
      ()
  | Some voxel -> 
      voxel.s <- v.s

(* [set_voxel_state] sets the voxel at given coordinates to the given state *)
let set_voxel_state 
  (m : t) 
  (x : int) 
  (y : int) 
  (s : Voxel.state) 
: unit =
  match get_voxel m x y with
  | None -> 
      ()
  | Some v -> 
      Voxel.set_state v s

let move_player
  (m : t) 
  (p : Player.t) 
: unit =
  let x = Player.get_x p in
  let y = Player.get_y p in
  let dir = Player.get_state p in
  let new_x =
    if dir = Player.Left then 
        x - 40
    else if dir = Player.Right then
        x + 40
    else 
        x
  in
  let new_y =
    if dir = Player.Up then 
        y + 40 
    else if dir = Player.Down then
        y - 40 else y
  in
  (* print_endline ((string_of_int x)^(string_of_int y)); *)
  match get_voxel m new_x new_y with
  | None -> 
      ()
  | Some v ->
      if Voxel.get_state v = Dot then (
        Player.set_x p new_x;
        Player.set_y p new_y;
        Player.inc_score p;
        set_voxel_state m x y Empty;
        set_voxel_state m new_x new_y Player
      )
      else if Voxel.get_state v = Bot then (
        Player.set_x p new_x;
        Player.set_y p new_y;
        m.ended <- true
      )
      else if Voxel.get_state v = Empty then (
        Player.set_x p new_x;
        Player.set_y p new_y;
        set_voxel_state m x y Empty;
        set_voxel_state m new_x new_y Player
      )

(* [move_bot] moves the bot in the given direction *)
let move_bot 
  (m : t) 
  (b : Bot.t) 
: unit =
  let x = Bot.get_x b in
  let y = Bot.get_y b in
  let dir = Bot.get_state b in
  let new_x =
    if dir = Bot.Left then 
        x - 40 
    else if dir = Bot.Right then
        x + 40 
    else 
        x
  in
  let new_y =
    if dir = Bot.Up then 
        y + 40 
    else if dir = Bot.Down then 
        y - 40 
    else 
        y
  in
  (* print_endline ((string_of_int x)^(string_of_int y)); *)
  match get_voxel m new_x new_y with
  | None -> 
      ()
  | Some v -> (
      if Voxel.get_state v = Dot then (
        match Random.int 15 with
        | 0 -> Bot.set_state b Left
        | 1 -> Bot.set_state b Right
        | 2 -> Bot.set_state b Up
        | 4 -> Bot.set_state b Down
        | _ ->
            Bot.set_x b new_x;
            Bot.set_y b new_y;
            set_voxel_state m x y Dot;
            set_voxel_state m new_x new_y Bot
      )
      else if Voxel.get_state v = Player then (
        Bot.set_x b new_x;
        Bot.set_y b new_y;
        m.ended <- true
      )
      else if Voxel.get_state v = Empty then (
        Bot.set_x b new_x;
        Bot.set_y b new_y;
        set_voxel_state m x y Empty;
        set_voxel_state m new_x new_y Bot
      )
      else if Voxel.get_state v = Bot then (
        Bot.set_x b new_x;
        Bot.set_y b new_y;
        set_voxel_state m x y Empty;
        set_voxel_state m new_x new_y Bot
      )
      else if dir = Up || dir = Down then
        match Random.int 2 with
        | 0 -> 
            Bot.set_state b Left
        | _ -> 
            Bot.set_state b Right
      else
        match Random.int 2 with
        | 0 -> 
            Bot.set_state b Up
        | _ -> 
            Bot.set_state b Down
  )

(* [get_player] returns the player voxel *)
let rec get_player 
  (m : t) 
: Voxel.t option =
  match m.voxels with
  | [] -> None
  | h :: t ->
      if Voxel.get_state h = Player then Some h
      else get_player { 
        voxels = t;
        ended = false
      }

(* [get_player_state] returns the state of the player voxel *)
let get_player_state 
  (m : t) 
: Voxel.state option =
  match get_player m with
  | None -> None
  | Some p -> Some p.s

(* [set_player] sets the player voxel to the given voxel *)
let set_player 
  (m : t) 
  (v : Voxel.t) 
: unit =
  match get_player m with
  | None -> ()
  | Some p ->
      Voxel.set_state p Empty;
      set_voxel m (Voxel.get_x v) (Voxel.get_y v) v

(* [set_player_state] sets the player voxel to the given state *)
let set_player_state 
(m : t) 
(s : Voxel.state) 
: unit =
  match get_player m with
  | None -> ()
  | Some p ->
      Voxel.set_state p s;
      set_voxel_state m (Voxel.get_x p) (Voxel.get_y p) s

(* [draw] draws the map *)
let rec draw 
  (m : t) 
  (p : Player.t) 
  (g1 : Bot.t) 
  (g2 : Bot.t)
 =
  match m.voxels with
  | [] -> 
      ()
  | h :: t ->
      Voxel.draw h;
      if Voxel.get_state h = Voxel.Player then (
        Player.draw_pacman p;
        draw { voxels = t; ended = false } p g1 g2
      )
      else if Voxel.get_state h = Voxel.Bot then
        if Bot.get_ghost g1 = Ghost1 then (
          Bot.draw_ghost g1;
          draw { voxels = t; ended = false } p g1 g2
          )
        else (
          Bot.draw_ghost g2;
          draw { voxels = t; ended = false } p g1 g2
        )
      else draw { voxels = t; ended = false } p g1 g2
