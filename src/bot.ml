type direction =
  | Up
  | Down
  | Left
  | Right

type mode =
  | Chase
  | Scatter
  | Frightened

type t = {
  color : Graphics.color;
  coor : int * int;
  dir : direction;
  mode : mode;
}

let draw_eye x y dir =
  Graphics.(
    set_color Graphics.white;
    fill_circle (x - 4) y 2;
    fill_circle (x + 4) y 2;
    if dir = Up then (
      fill_circle (x - 4) (y + 1) 1;
      fill_circle (x + 4) (y + 1) 1)
    else if dir = Down then (
      fill_circle (x - 4) (y - 1) 1;
      fill_circle (x + 4) (y - 1) 1)
    else if dir = Right then (
      fill_circle (x - 5) y 1;
      fill_circle (x + 3) y 1)
    else if dir = Left then (
      fill_circle (x - 3) y 1;
      fill_circle (x + 5) y 1))

let draw_ghost g =
  Graphics.(
    set_color g.color;
    fill_circle (fst g.coor) (snd g.coor) 8;
    fill_rect (fst g.coor - 8) (snd g.coor - 10) 16 10;
    draw_eye (fst g.coor) (snd g.coor) g.dir)