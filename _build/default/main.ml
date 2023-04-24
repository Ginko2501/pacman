open Graphics
open Game

(* Types *)

(* Constants *)
let screen_width = 640
let screen_height = 480
let wall = 10

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

let draw_wall (x, y, len, dir) =
  set_color blue;
  (* dir = 0 means vertical and dir = 1 means horizontal *)
  if dir = 0 then draw_rect x y wall len
  else if dir = 1 then draw_rect x y len wall

let map =
  [
    (wall, wall, screen_width - (wall * 2), 1);
    (wall, wall, screen_height - (wall * 2), 0);
    (wall, screen_height - (wall * 2), screen_width - (wall * 2), 1);
    (screen_width - (wall * 2), wall, screen_height - (wall * 2), 0);
    ((screen_width / 2) - 50, (screen_height / 2) - 25, 100, 1);
    ((screen_width / 2) - 50, (screen_height / 2) + 25, 40, 1);
    ((screen_width / 2) + 20, (screen_height / 2) + 25, 40, 1);
    ((screen_width / 2) - 50, (screen_height / 2) - 25, 50, 0);
    ((screen_width / 2) + 50, (screen_height / 2) - 25, 50, 0);
    ((screen_width / 2) - 2, 10, 100, 0);
    ((screen_width / 2) - 2, screen_height - 110, 100, 0);
    (130, 100, 60, 1);
    (130, 100, 100, 0);
    (130, 280, 100, 0);
    (130, 380, 60, 1);
    (screen_width - 130 - 60, 100, 60, 1);
    (screen_width - 130, 100, 100, 0);
    (screen_width - 130, 280, 100, 0);
    (screen_width - 130 - 50, 380, 60, 1);
  ]

let draw_pacman (x, y) a1 a2 =
  set_color yellow;
  fill_circle x y 20;
  (* draw with background color*)
  set_color black;
  fill_arc x y 20 20 a1 a2

let draw_dot (x, y) =
  set_color white;
  fill_circle x y 5

let dots =
  [
    (200, 150);
    (200, 200);
    (250, 150);
    (300, 150);
    (60, 60);
    (110, 60);
    (160, 60);
    (210, 60);
    (260, 60);
    (60, 110);
    (60, 160);
    (60, 210);
  ]

let dots2 = List.map (fun (x, y) -> (screen_width - x, y)) dots
let dots3 = List.map (fun (x, y) -> (screen_width - x, screen_height - y)) dots
let dots4 = List.map (fun (x, y) -> (x, screen_height - y)) dots
let draw_dots li = List.iter draw_dot li

let draw_map () =
  let bg_color = black in
  set_color bg_color;
  fill_rect 0 0 screen_width screen_height;
  List.iter draw_wall map;
  draw_pacman (200, 230) ~-27 27;
  draw_dots dots;
  draw_dots dots2;
  draw_dots dots3;
  draw_dots dots4

let rec wait_for_start_click () =
  if button_down () then
    let x, y = mouse_pos () in
    if
      x > (screen_width / 2) - 50
      && x < (screen_width / 2) + 50
      && y > (screen_height / 2) - 50
      && y < screen_height / 2
    then draw_map ()
    else wait_for_start_click ()
  else wait_for_start_click ()

(* type state = Player | Bot | Wall | Empty | Dot *)


let main () =
  init_graphics ();
  draw_start_screen ();
  let v = Voxel.make 100 100 Player in 
  Voxel.plot v;
  wait_for_start_click ()
(* Enter the actual game scene here *)

(* Run the program *)
let () = main ()