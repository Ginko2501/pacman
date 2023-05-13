open Graphics
open Pacman

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
  draw_string "Start Game";

  let rule_color = rgb 255 255 0 in
  set_color rule_color;
  set_text_size 20;
  moveto ((screen_width / 2) - 200) ((screen_height / 2) - 70);
  draw_string
    "Use W A S D to control the PACMAN. Get as many points as you can!"

(* draw the end screen *)
(* this function is called once at the end of the program *)
let draw_end_screen () =
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

  let dead_color = red in
  set_color dead_color;
  set_text_size 50;
  moveto ((screen_width / 2) - 40) ((screen_height / 2) + 10);
  draw_string "You are dead...";

  let button_color = rgb 255 0 0 in
  set_color button_color;
  fill_rect ((screen_width / 2) - 50) ((screen_height / 2) - 50) 100 50;
  let button_text_color = rgb 255 255 255 in
  set_color button_text_color;

  moveto ((screen_width / 2) - 40) ((screen_height / 2) - 30);
  draw_string "Play Again"

(* draw the congratulations screen *)
(* this function is called once if player's points = 255 *)
let draw_cong_screen () =
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

  let dead_color = red in
  set_color dead_color;
  set_text_size 50;
  moveto ((screen_width / 2) - 50) ((screen_height / 2) + 10);
  draw_string "Conguatulations! You win!";

  let button_color = rgb 255 0 0 in
  set_color button_color;
  fill_rect ((screen_width / 2) - 50) ((screen_height / 2) - 50) 100 50;
  let button_text_color = rgb 255 255 255 in
  set_color button_text_color;

  moveto ((screen_width / 2) - 40) ((screen_height / 2) - 30);
  draw_string "Play Again"

(* [map] is the map of the game *)
let map = Map.init ()
let player = Player.init 60 60
let bot1 = Bot.init_ghost 100 740 Ghost1
let bot2 = Bot.init_ghost 740 740 Ghost2

(* draw the map *)
(* this function is called every iteration of the main loop *)
let draw_game () =
  Map.draw map player bot1 bot2;
  (* draw the score at the right top *)
  (* string size is not changed now *)
  let score_color = rgb 255 255 255 in
  set_color score_color;
  set_text_size 40;
  (* does not work right now *)
  moveto (screen_width - 100) (screen_height - 50);
  draw_string ("Score: " ^ string_of_int (Player.get_score player))

(* [started] is whether the game has started *)
let started = ref false

(* check if the start button has been clicked *)
(* this function is called every iteration of the main loop *)
let read_click () =
  if button_down () then
    let x, y = mouse_pos () in
    if
      x > (screen_width / 2) - 50
      && x < (screen_width / 2) + 50
      && y > (screen_height / 2) - 50
      && y < screen_height / 2
    then started := true
    else ()
  else ()

(* test set_voxel_state let v = Map.get_voxel map 0 0 in match v with | None ->
   print_endline "None" | Some v -> print_endline (Voxel.string_of_state v.s);
   Map.set_voxel_state map 0 0 Voxel.Empty; let v = Map.get_voxel map 0 0 in
   match v with | None -> print_endline "None" | Some v -> print_endline
   (Voxel.string_of_state v.s); Unix.sleepf 10.0; *)

let read_keys () =
  if key_pressed () then
    let key = read_key () in
    if key = 'd' then 
        Player.set_state player Right
    else if key = 'a' then 
        Player.set_state player Left
    else if key = 'w' then 
        Player.set_state player Up
    else if key = 's' then 
        Player.set_state player Down
    else 
        ()

(* main function *)
(* this function is called once at the beginning of the program *)
let rec main () =
  Unix.sleepf 0.2;
  clear ();
  if !started then (
    draw_game ();
    read_keys ();
    Map.move_player map player;
    Map.move_bot map bot1;
    Map.move_bot map bot2;
    if map.ended then started := false
    else if Player.get_score player = 100 then (
      draw_cong_screen ();
      Unix.sleepf 4.0;
      Map.reset map;
      Player.set_x player 60;
      Player.set_y player 60;
      Player.set_state player Right;
      Bot.set_x bot1 740;
      Bot.set_y bot1 100;
      Bot.set_x bot2 740;
      Bot.set_y bot2 740;
      Player.set_score player 0)
    else (
      read_click ();
      main ()))
  else if map.ended then (
    draw_end_screen ();
    Unix.sleepf 3.0;
    map.ended <- false;
    Map.reset map;
    Player.set_x player 60;
    Player.set_y player 60;
    Player.set_state player Right;
    Bot.set_x bot1 740;
    Bot.set_y bot1 100;
    Bot.set_x bot2 740;
    Bot.set_y bot2 740;
    Player.set_score player 0)
  else draw_start_screen ();
  read_click ();
  main ()

(* Run the program *)
let () =
  init_graphics ();
  main ()
