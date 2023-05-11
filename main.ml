open Graphics
open Game

let screen_width = 800
let screen_height = 800

(* initialize the graphics window *)
(* this function is called once at the beginning of the program *)
let init_graphics () =
  open_graph "";
  set_window_title "Pac-Man";
  resize_window screen_width screen_height

let clear () =
  let bg_color = black in
  set_color bg_color;
  fill_rect 0 0 screen_width screen_height

(* draw the start screen *)
(* this function is called once at the beginning of the program *)
let draw_start_screen () =
    (* draw the background *)
    let bg_color = black in
    set_color bg_color;
    fill_rect 0 0 screen_width screen_height;

    (* draw the title *)
    let title_color = rgb 255 255 0 in
    set_color title_color;
    set_text_size 20;
    moveto ((screen_width / 2) - 200) ((screen_height / 2) + 100);
    draw_string "PAC-MAN";

    (* draw the start button *)
    let button_color = rgb 255 0 0 in
    set_color button_color;
    fill_rect ((screen_width / 2) - 50) ((screen_height / 2) - 50) 100 50;
    let button_text_color = rgb 255 255 255 in
    set_color button_text_color;

    (* draw the start button text *)
    moveto ((screen_width / 2) - 40) ((screen_height / 2) - 30);
    draw_string "Start Game"

(* [map] is the map of the game *)
let map = Map.init()

let player = Player.init 60 60

(* draw the map *)
(* this function is called every iteration of the main loop *)
let draw_map () = 
  Map.draw map

(* [started] is whether the game has started *)
let started = ref false

(* check if the start button has been clicked *)
(* this function is called every iteration of the main loop *)
let read_click () = 
  if button_down () then
    let x, y = mouse_pos () in
    if x > (screen_width / 2) - 50
    && x < (screen_width / 2) + 50
    && y > (screen_height / 2) - 50
    && y < screen_height / 2
    then started := true
    else ()
  else ()

(* test set_voxel_state
    let v = Map.get_voxel map 0 0 in
    match v with 
    | None -> print_endline "None"
    | Some v -> print_endline (Voxel.string_of_state v.s);
    Map.set_voxel_state map 0 0 Voxel.Empty;
    let v = Map.get_voxel map 0 0 in
    match v with 
    | None -> print_endline "None"
    | Some v -> print_endline (Voxel.string_of_state v.s);
    Unix.sleepf 10.0;   
*)

(* let self_empty map p = Player.(Map.set_voxel_state map p.x p.y Empty)

let move_pacman map p = 
   let key = read_key () in
      if key = 'd' then 
        (print_endline "here";
        (Player.set_state p Right;
        (if Map.get_voxel_state map (p.x+1) p.y = Some Wall then ()
        else self_empty map p; (Map.set_voxel_state map (p.x+1) p.y Player))))
      else if key = 'a' then 
        (Player.set_state p Left;
        (if Map.get_voxel_state map (p.x-1) p.y = Some Wall then ()
        else self_empty map p; (Map.set_voxel_state map (p.x-1) p.y Player)))
      else if key = 'w' then 
        (Player.set_state p Up;
        (if Map.get_voxel_state map p.x (p.y+1) = Some Wall then ()
        else self_empty map p; (Map.set_voxel_state map p.x (p.y+1) Player)))
      else if key = 's' then 
        (Player.set_state p Down;
        (if Map.get_voxel_state map p.x (p.y-1) = Some Wall then ()
        else self_empty map p; (Map.set_voxel_state map p.x (p.y-1) Player)))
      else () *)

let read_keys () = 
  let key = read_key () in
  if key = 'd' then 
    Player.set_state player Right
  else if key = 'a' then 
    Player.set_state player Left
  else if key = 'w' then 
    Player.set_state player Up
  else if key = 's' then 
    Player.set_state player Down
  else ()

(* main function *)
(* this function is called once at the beginning of the program *)
let rec main () = 
  Unix.sleepf 0.2;
  clear ();
  if !started then (
    draw_map ();
    read_keys ();
    Map.move_player map player;
    main ())
  else
    draw_start_screen ();
    read_click ();
    main ()
    

(* Run the program *)
let () = 
  init_graphics ();
  main ()