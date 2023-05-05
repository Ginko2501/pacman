open Graphics
open Game

(* Types *)

(* Constants *)
let screen_width = 640
let screen_height = 480

(* Functions *)
let init_graphics () =
  open_graph "";
  set_window_title "Pac-Man";
  resize_window screen_width screen_height

let draw_start_screen () =
    let bg_color = black in
    set_color bg_color;
    fill_rect 0 0 screen_width screen_height;
    let title_color = rgb 255 255 0 in
    set_color title_color;
    set_text_size 20;
    moveto ((screen_width / 2) - 200) ((screen_height / 2) + 100);
    draw_string "PAC-MAN";
    let button_color = rgb 255 0 0 in
    set_color button_color;
    fill_rect ((screen_width / 2) - 50) ((screen_height / 2) - 50) 100 50;
    let button_text_color = rgb 255 255 255 in
    set_color button_text_color;
    moveto ((screen_width / 2) - 40) ((screen_height / 2) - 30);
    draw_string "Start Game"

let draw_map () =
  let bg_color = black in
  set_color bg_color;
  fill_rect 0 0 screen_width screen_height;
  let a = List.map (Voxel.make Voxel.Wall) Map.map_wall in
  List.iter Voxel.plot a;
  Voxel.(make Player (60, 60) |> plot) 

let rec wait_for_start_click () =
  if button_down () then
    let x, y = mouse_pos () in
    if
      x > (screen_width / 2) - 50
      && x < (screen_width / 2) + 50
      && y > (screen_height / 2) - 50
      && y < screen_height / 2
    then ()
    else wait_for_start_click ()
  else wait_for_start_click ()

(* type state = Player | Bot | Wall | Empty | Dot *)


let main () =
  init_graphics ();
  draw_map ();
  wait_for_start_click ();
  draw_start_screen ()
(* Enter the actual game scene here *)

(* Run the program *)
let () = main ()