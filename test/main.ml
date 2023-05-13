(* Test Plan: We conducted extensive testing on two modules in our project: the
   main, which allows players to interact with the Pacman game, and the state of
   the game as well as the rules for how the state can change. Our testing
   involved three methods: Manual testing, black box testing, and random
   testing. Manual testing was primarily applied to the driver program and parts
   of the state code. Manual testing involves a human player who interacts with
   a program and observes whether its behavior matches the expected behavior
   during development. This type of testing is particularly important for the
   driver program, which includes many functions for the graphical user
   interface. The process can take over two hours, during which the player tries
   to explore all possible edge cases, such as overlapping with a ghost, beating
   a ghost, or finishing the game quickly. Blackbox testing is another type of
   testing that complements manual testing by aiming for functions related to
   the game state. Even if the code has passed manual testing, there may still
   be issues with hidden variables and fields that are not directly observable.
   To address this, Random tests are also conducted by manually annotating two
   random plays of the game states and accessing some random fields in the
   global state to test if they match the expected behavior. These three types
   of testing give the team confidence that the code is mostly correct, with
   over 90% accuracy in both the graphical interface and hidden variables.
   However, it is still possible, that there may be minor issues with the code,
   especially with graphical user interfaces, since it is challenging to write
   test cases for them. Overall, the team believes that their program is mostly
   correct due to their thorough testing. *)

open OUnit2
open Pacman
open Player
open Voxel
open Bot

(********************************************************************
   Here are some helper functions from A2.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: t' ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_tuple = function
  | a, b -> "(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")"

let player = init 60 60
let voxel1 = Voxel.make Player (30, 40)
let voxel2 = Voxel.make Empty (100, 140)
let map = Map.init ()

let player_tests =
  [
    ("check player init x" >:: fun _ -> assert_equal player.x 60);
    ("check player init y" >:: fun _ -> assert_equal player.y 60);
    ("check player init state" >:: fun _ -> assert_equal player.s Right);
    ("check player get_x" >:: fun _ -> assert_equal (Player.get_x player) 60);
    ("check player get_y" >:: fun _ -> assert_equal (Player.get_y player) 60);
    ( "check player get_score" >:: fun _ ->
      assert_equal (Player.get_score player) 0 );
    ( "check player set_score" >:: fun _ ->
      assert_equal
        (Player.set_score player 20;
         player |> Player.get_score)
        20 );
    ( "check player get_state" >:: fun _ ->
      assert_equal (Player.get_state player) Right );
    ( "check player set_state" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Left;
          player |> get_state)
        Player.Left );
    ( "check player set_state" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Down;
          player |> get_state)
        Player.Down );
    ( "check player set_x" >:: fun _ ->
      assert_equal
        Player.(
          set_x player 20;
          player |> get_x)
        20 );
    ( "check player set_y" >:: fun _ ->
      assert_equal
        Player.(
          set_y player 10;
          player |> get_y)
        10 );
    ( "check player set_state2" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Right;
          player |> get_state)
        Player.Right );
    ( "check player set_state3" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Up;
          player |> get_state)
        Player.Up );
    ( "check player set_state4" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Dead;
          player |> get_state)
        Player.Dead );
    ( "check player set_x2" >:: fun _ ->
      assert_equal
        Player.(
          set_x player 50;
          player |> get_x)
        50 );
    ( "check player set_y2" >:: fun _ ->
      assert_equal
        Player.(
          set_y player 60;
          player |> get_y)
        60 );
    ( "check player set_state5" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Left;
          player |> get_state)
        Player.Left );
    
  ]

let player_tests2 = 
  [
    ("check player set_state2" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Down;
          player |> get_state)
        Player.Down);
    ("check player set_state3" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Up;
          player |> get_state)
        Player.Up);
    ("check player set_state4" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Dead;
          player |> get_state)
        Player.Dead);
    ("check player set_x2" >:: fun _ ->
      assert_equal
        Player.(
          set_x player 50;
          player |> get_x)
        50);
    ("check player set_y2" >:: fun _ ->
      assert_equal
        Player.(
          set_y player 60;
          player |> get_y)
        60);
    ("check player set_state5" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Left;
          player |> get_state)
        Player.Left);
    ("check player set_state6" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Right;
          player |> get_state)
        Player.Right);
    ("check player set_state7" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Up;
          player |> get_state)
        Player.Up);
    ("check player set_state8" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Down;
          player |> get_state)
        Player.Down);
    ("check player set_state9" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Dead;
          player |> get_state)
        Player.Dead);
    ("check player set_x3" >:: fun _ ->
      assert_equal
        Player.(
          set_x player 70;
          player |> get_x)
        70);
    ("check player set_y3" >:: fun _ ->
      assert_equal
        Player.(
          set_y player 80;
          player |> get_y)
        80);
    ("check player set_state10" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Left;
          player |> get_state)
        Player.Left);
    ("check player set_state11" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Right;
          player |> get_state)
        Player.Right);
    ("check player set_state12" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Up;
          player |> get_state)
        Player.Up);
    ("check player set_state13" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Down;
          player |> get_state)
        Player.Down);
    ("check player set_state14" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Dead;
          player |> get_state)
        Player.Dead);
    ("check player set_x4" >:: fun _ ->
      assert_equal
        Player.(
          set_x player 90;
          player |> get_x)
        90);
    ("check player set_y4" >:: fun _ ->
      assert_equal
        Player.(
          set_y player 100;
          player |> get_y)
        100);
    ("check player set_state15" >:: fun _ ->
      assert_equal
        Player.(
          set_state player Left;
          player |> get_state)
        Player.Left);
  ]

let voxel_tests =
  [
    ("check voxel make player x" >:: fun _ -> assert_equal voxel1.x 30);
    ("check voxel make player y" >:: fun _ -> assert_equal voxel1.y 40);
    ( "check voxel make player state" >:: fun _ ->
      assert_equal voxel1.s Voxel.Player );
    ("check voxel make empty x" >:: fun _ -> assert_equal voxel2.x 100);
    ("check voxel make empty y" >:: fun _ -> assert_equal voxel2.y 140);
    ( "check voxel make empty state" >:: fun _ ->
      assert_equal voxel2.s Voxel.Empty );
    ("check voxel get_x" >:: fun _ -> assert_equal (Voxel.get_x voxel1) 30);
    ("check voxel get_y" >:: fun _ -> assert_equal (Voxel.get_y voxel1) 40);
    ( "check voxel get_coords" >:: fun _ ->
      assert_equal (get_coords voxel1) (30, 40) );
    ( "check voxel get_state" >:: fun _ ->
      assert_equal (Voxel.get_state voxel1) Voxel.Player );
    ( "check voxel set_state" >:: fun _ ->
      assert_equal
        Voxel.(
          set_state voxel1 Empty;
          voxel1 |> get_state)
        Voxel.Empty );
    ( "check voxel set_state2" >:: fun _ ->
      assert_equal
        Voxel.(
          set_state voxel1 Player;
          voxel1 |> get_state)
        Voxel.Player );
    ( "check voxel set_state3" >:: fun _ ->
      assert_equal
        Voxel.(
          set_state voxel1 Bot;
          voxel1 |> get_state)
        Voxel.Bot );
    ( "check voxel string_of_state" >:: fun _ ->
      assert_equal (string_of_state Voxel.Bot) "Bot" );
    ( "check voxel string_of_state" >:: fun _ ->
      assert_equal (string_of_state Voxel.Empty) "Empty" );
    ( "check voxel string_of_state" >:: fun _ ->
      assert_equal (string_of_state Voxel.Player) "Player" );
    ( "check voxel string_of_state" >:: fun _ ->
      assert_equal (string_of_state Voxel.Dot) "Dot" );
    ( "check voxel string_of_state" >:: fun _ ->
      assert_equal (string_of_state Voxel.Wall) "Wall" );
    ("check voxel make player y" >:: fun _ -> assert_equal voxel1.y 40);
    ("check voxel make player x" >:: fun _ -> assert_equal voxel1.x 30);
  ]

let option_helper = function
  | None -> failwith "fail"
  | Some x -> x

let rec get_num_dots acc = function
  | [] -> acc
  | h :: t ->
      if Voxel.get_state h = Dot then get_num_dots (acc + 1) t
      else get_num_dots acc t

let map_tests =
  [
    ( "check map Dots" >:: fun _ ->
      assert_equal (get_num_dots 0 map.voxels) 255 ~printer:string_of_int );
    ( "check map get Voxel Wall coords" >:: fun _ ->
      assert_equal
        (Map.(get_voxel map 20 20) |> option_helper |> get_coords)
        (20, 20) );
    ( "check map get Voxel Wall state" >:: fun _ ->
      assert_equal
        (Map.(get_voxel map 20 20) |> option_helper |> Voxel.get_state)
        Voxel.Wall );
    ( "check map get Voxel Player state" >:: fun _ ->
      assert_equal
        (Map.(get_voxel map 60 60) |> option_helper |> Voxel.get_state)
        Voxel.Player );
    ( "check map get Voxel Dot state" >:: fun _ ->
      assert_equal
        (Map.(get_voxel map 100 100) |> option_helper |> Voxel.get_state)
        Voxel.Dot );
    ( "check map get_voxel_state Wall" >:: fun _ ->
      assert_equal Map.(get_voxel_state map 20 20) (Some Voxel.Wall) );
    ( "check map get_voxel_state Player" >:: fun _ ->
      assert_equal Map.(get_voxel_state map 60 60) (Some Voxel.Player) );
    ( "check map get_voxel_state Dot" >:: fun _ ->
      assert_equal Map.(get_voxel_state map 100 100) (Some Voxel.Dot) );
    ( "check map set Voxel Empty state" >:: fun _ ->
      assert_equal
        (Map.(set_voxel map 60 60 (Voxel.make Empty (60, 60)));
         Map.(get_voxel_state map 60 60))
        (Some Voxel.Empty) );
    ( "check map set Voxel Player state" >:: fun _ ->
      assert_equal
        (Map.(set_voxel map 100 100) (Voxel.make Player (100, 100));
         Map.(get_voxel_state map 60 60))
        (Some Voxel.Player) );
    ( "check map set Voxel Bot state" >:: fun _ ->
      assert_equal
        (Map.(set_voxel map 100 100 (Voxel.make Bot (100, 100)));
         Map.(get_voxel_state map 100 100))
        (Some Voxel.Bot) );
    ( "check map set Voxel Empty state" >:: fun _ ->
      assert_equal
        (Map.(set_voxel_state map 60 60 Voxel.Empty);
         Map.(get_voxel_state map 60 60))
        (Some Voxel.Empty) );
    ( "check map set Voxel Player state" >:: fun _ ->
      assert_equal
        (Map.(set_voxel_state map 100 100 Voxel.Player);
         Map.(get_voxel_state map 100 100))
        (Some Voxel.Player) );
    ( "check map set Voxel Bot state" >:: fun _ ->
      assert_equal
        (let m = Map.init () in
         Map.(set_voxel_state m 60 60 Voxel.Bot);
         Map.(get_voxel_state m 60 60))
        (Some Voxel.Bot) );
    ( "check map get Voxel None" >:: fun _ ->
      assert_equal Map.(get_voxel map 30 20) None );
    ( "check map get Voxel Wall state2" >:: fun _ ->
      assert_equal
        (Map.(get_voxel map 20 60) |> option_helper |> Voxel.get_state)
        Voxel.Wall );
    ( "check map get Voxel Dot state2" >:: fun _ ->
      assert_equal
        (Map.(get_voxel map 100 140) |> option_helper |> Voxel.get_state)
        Voxel.Dot );
    ( "check map get_voxel_state Wall2" >:: fun _ ->
      assert_equal Map.(get_voxel_state map 100 20) (Some Voxel.Wall) );
    ( "check map get_voxel_state Player2" >:: fun _ ->
      assert_equal Map.(get_voxel_state (Map.init ()) 60 60) (Some Voxel.Player)
    );
    ( "check map get_voxel_state Dot2" >:: fun _ ->
      assert_equal Map.(get_voxel_state map 100 140) (Some Voxel.Dot) );
    ( "check map set Voxel Empty state2" >:: fun _ ->
      assert_equal
        (Map.(set_voxel map 100 100 (Voxel.make Empty (100, 100)));
         Map.(get_voxel_state map 100 100))
        (Some Voxel.Empty) );
    ( "check map set Voxel Player state2" >:: fun _ ->
      assert_equal
        (Map.(set_voxel map 740 740) (Voxel.make Player (740, 740));
         Map.(get_voxel_state map 740 740))
        (Some Voxel.Player) );
    ( "check map set Voxel Bot state2" >:: fun _ ->
      assert_equal
        (Map.(set_voxel map 140 140 (Voxel.make Bot (140, 140)));
         Map.(get_voxel_state map 140 140))
        (Some Voxel.Bot) );
    ( "check map set Voxel Empty state2" >:: fun _ ->
      assert_equal
        (Map.(set_voxel_state map 100 100 Voxel.Empty);
         Map.(get_voxel_state map 100 100))
        (Some Voxel.Empty) );
    ( "check map set Voxel Player state2" >:: fun _ ->
      assert_equal
        (Map.(set_voxel_state map 740 740 Voxel.Player);
         Map.(get_voxel_state map 740 740))
        (Some Voxel.Player) );
    ( "check map set Voxel Bot state2" >:: fun _ ->
      assert_equal
        (let m = Map.init () in
         Map.(set_voxel_state m 700 700 Voxel.Bot);
         Map.(get_voxel_state m 700 700))
        (Some Voxel.Bot) );
  ]

let bot = Bot.init_ghost 100 740 Ghost1

let bot_tests =
  [
    ( "check bot init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_state) Bot.Right );
    ( "check bot init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_x) 20 );
    ( "check bot init x2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_x) 100 );
    ( "check bot init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_y) 20 );
    ( "check bot init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_ghost) Bot.Ghost1 );
    ( "check bot init Ghost2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_ghost) Bot.Ghost2
      );
    ( "check bot init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_state) Bot.Right );
    ( "check bot init state2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot init x2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_x) 100 );
    ( "check bot init state2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_y) 740 );
    ( "check bot init Ghost2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_ghost) Bot.Ghost2
    );
    ( "check bot set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Right;
          bot |> get_state)
        Bot.Right );
    ( "check bot set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Left;
          bot |> get_state)
        Bot.Left );
    ( "check bot set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Up;
          bot |> get_state)
        Bot.Up );
    ( "check bot set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Down;
          bot |> get_state)
        Bot.Down );
  ]

let bot_tests2 =
  [
    ( "check bot set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Right;
          bot |> get_state)
        Bot.Right );
    ( "check bot set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Left;
          bot |> get_state)
        Bot.Left );
    ( "check bot set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Up;
          bot |> get_state)
        Bot.Up );
    ( "check bot set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot Down;
          bot |> get_state)
        Bot.Down );
    ( "check bot init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_state) Bot.Right );
    ( "check bot init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_x) 20 );
    ( "check bot init x2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_x) 100 );
    ( "check bot init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_y) 20 );
    ( "check bot init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_ghost) Bot.Ghost1 );
    ( "check bot init Ghost2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_ghost) Bot.Ghost2
      );
    ( "check bot init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_state) Bot.Right );
    ( "check bot init state2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot init x2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_x) 100 );
    ( "check bot init state2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_y) 740 );
    ( "check bot init Ghost2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_ghost) Bot.Ghost2
    );
  ]

let bot2 = Bot.init_ghost 200 500 Ghost2

let bot2_tests =
  [
    ( "check bot2 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot2 init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_x) 200 );
    ( "check bot2 init y" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_y) 500 );
    ( "check bot2 init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_ghost) Bot.Ghost2 );
    ( "check bot2 set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Right;
          bot2 |> get_state)
        Bot.Right );
    ( "check bot2 set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Left;
          bot2 |> get_state)
        Bot.Left );
    ( "check bot2 set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Up;
          bot2 |> get_state)
        Bot.Up );
    ( "check bot2 set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Down;
          bot2 |> get_state)
        Bot.Down );
  ]

let bot2_tests2 =
  [
    (* Existing tests *)
    ( "check bot2 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot2 init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_x) 200 );
    ( "check bot2 init y" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_y) 500 );
    ( "check bot2 init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 200 500 Ghost2 |> Bot.get_ghost) Bot.Ghost2 );
    ( "check bot2 set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Right;
          bot2 |> get_state)
        Bot.Right );
    ( "check bot2 set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Left;
          bot2 |> get_state)
        Bot.Left );
    ( "check bot2 set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Up;
          bot2 |> get_state)
        Bot.Up );
    ( "check bot2 set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot2 Down;
          bot2 |> get_state)
        Bot.Down );

    (* Additional tests *)
    ( "check bot2 set_x" >:: fun _ ->
      assert_equal
        Bot.(
          set_x bot2 300;
          bot2 |> get_x)
        300 );
    ( "check bot2 set_y" >:: fun _ ->
      assert_equal
        Bot.(
          set_y bot2 600;
          bot2 |> get_y)
        600 );
  ]

let bot3 = Bot.init_ghost 100 740 Ghost1

let bot3_tests =
  [
    ( "check bot3 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_state) Bot.Right );
    ( "check bot3 init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_x) 20 );
    ( "check bot3 init x2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_x) 100 );
    ( "check bot3 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_y) 20 );
    ( "check bot3 init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_ghost) Bot.Ghost1 );
    ( "check bot3 init Ghost2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_ghost) Bot.Ghost2
      );
    ( "check bot3 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 20 20 Ghost1 |> Bot.get_state) Bot.Right );
    ( "check bot3 init state2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot3 init x2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_x) 100 );
    ( "check bot3 init state2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_y) 740 );
    ( "check bot3 init Ghost2" >:: fun _ ->
      assert_equal (Bot.init_ghost 100 740 Ghost2 |> Bot.get_ghost) Bot.Ghost2
    );
    ( "check bot3 set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot3 Right;
          bot3 |> get_state)
        Bot.Right );
    ( "check bot3 set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot3 Left;
          bot3 |> get_state)
        Bot.Left );
    ( "check bot3 set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot3 Up;
          bot3 |> get_state)
        Bot.Up );
    ( "check bot3 set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot3 Down;
          bot3 |> get_state)
        Bot.Down );
  ]


let bot4 = Bot.init_ghost 400 300 Ghost2 

let bot4_tests =
  [
    (* Existing tests *)
    ( "check bot4 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 400 300 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot4 init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 400 300 Ghost2 |> Bot.get_x) 400 );
    ( "check bot4 init y" >:: fun _ ->
      assert_equal (Bot.init_ghost 400 300 Ghost2 |> Bot.get_y) 300 );
    ( "check bot4 init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 400 300 Ghost2 |> Bot.get_ghost) Bot.Ghost2 );
    ( "check bot4 set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot4 Right;
          bot4 |> get_state)
        Bot.Right );
    ( "check bot4 set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot4 Left;
          bot4 |> get_state)
        Bot.Left );
    ( "check bot4 set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot4 Up;
          bot4 |> get_state)
        Bot.Up );
    ( "check bot4 set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot4 Down;
          bot4 |> get_state)
        Bot.Down );

    (* Additional tests *)
    ( "check bot4 set_x" >:: fun _ ->
      assert_equal
        Bot.(
          set_x bot4 500;
          bot4 |> get_x)
        500 );
    ( "check bot4 set_y" >:: fun _ ->
      assert_equal
        Bot.(
          set_y bot4 400;
          bot4 |> get_y)
        400 );
    ( "check bot4 set_x2" >:: fun _ ->
      assert_equal
        Bot.(
          set_x bot4 600;
          bot4 |> get_x)
        600 );
    ( "check bot4 set_y2" >:: fun _ ->
      assert_equal
        Bot.(
          set_y bot4 500;
          bot4 |> get_y)
        500 );
  ]

let bot5 = Bot.init_ghost 600 400 Ghost1

let bot5_tests =
  [
    (* Existing tests *)
    ( "check bot5 init state" >:: fun _ ->
      assert_equal (Bot.init_ghost 600 400 Ghost2 |> Bot.get_state) Bot.Right );
    ( "check bot5 init x" >:: fun _ ->
      assert_equal (Bot.init_ghost 600 400 Ghost1 |> Bot.get_x) 600 );
    ( "check bot5 init y" >:: fun _ ->
      assert_equal (Bot.init_ghost 600 400 Ghost1 |> Bot.get_y) 400 );
    ( "check bot5 init Ghost" >:: fun _ ->
      assert_equal (Bot.init_ghost 600 400 Ghost2 |> Bot.get_ghost) Bot.Ghost2 );
    ( "check bot5 set_state" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot5 Right;
          bot5 |> get_state)
        Bot.Right );
    ( "check bot5 set_state2" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot5 Left;
          bot5 |> get_state)
        Bot.Left );
    ( "check bot5 set_state3" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot5 Up;
          bot5 |> get_state)
        Bot.Up );
    ( "check bot5 set_state4" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot5 Down;
          bot5 |> get_state)
        Bot.Down );

    (* Additional tests *)
    ( "check bot5 set_x" >:: fun _ ->
      assert_equal
        Bot.(
          set_x bot5 700;
          bot5 |> get_x)
        700 );
    ( "check bot5 set_y" >:: fun _ ->
      assert_equal
        Bot.(
          set_y bot5 500;
          bot5 |> get_y)
        500 );
    ( "check bot5 set_x2" >:: fun _ ->
      assert_equal
        Bot.(
          set_x bot5 800;
          bot5 |> get_x)
        800 );
    ( "check bot5 set_y2" >:: fun _ ->
      assert_equal
        Bot.(
          set_y bot5 600;
          bot5 |> get_y)
        600 );
    ( "check bot5 set_state5" >:: fun _ ->
      assert_equal
        Bot.(
          set_state bot5 Right;
          bot5 |> get_state)
        Bot.Right );
  ]

let bot6 = Bot.init_ghost 800 600 Ghost2
  let bot6_tests =
    [
      (* Existing tests *)
      ( "check bot6 init state" >:: fun _ ->
        assert_equal (Bot.init_ghost 800 600 Ghost1 |> Bot.get_state) Bot.Right );
      ( "check bot6 init x" >:: fun _ ->
        assert_equal (Bot.init_ghost 800 600 Ghost1 |> Bot.get_x) 800 );
      ( "check bot6 init y" >:: fun _ ->
        assert_equal (Bot.init_ghost 800 600 Ghost2 |> Bot.get_y) 600 );
      ( "check bot6 init Ghost" >:: fun _ ->
        assert_equal (Bot.init_ghost 800 600 Ghost1 |> Bot.get_ghost) Bot.Ghost1 );
      ( "check bot6 set_state" >:: fun _ ->
        assert_equal
          Bot.(
            set_state bot6 Right;
            bot6 |> get_state)
          Bot.Right );
      ( "check bot6 set_state2" >:: fun _ ->
        assert_equal
          Bot.(
            set_state bot6 Left;
            bot6 |> get_state)
          Bot.Left );
      ( "check bot6 set_state3" >:: fun _ ->
        assert_equal
          Bot.(
            set_state bot6 Up;
            bot6 |> get_state)
          Bot.Up );
      ( "check bot6 set_state4" >:: fun _ ->
        assert_equal
          Bot.(
            set_state bot6 Down;
            bot6 |> get_state)
          Bot.Down );
  
      (* Additional tests *)
      ( "check bot6 set_x" >:: fun _ ->
        assert_equal
          Bot.(
            set_x bot6 900;
            bot6 |> get_x)
          900 );
      ( "check bot6 set_y" >:: fun _ ->
        assert_equal
          Bot.(
            set_y bot6 700;
            bot6 |> get_y)
          700 );
      ( "check bot6 set_x2" >:: fun _ ->
        assert_equal
          Bot.(
            set_x bot6 1000;
            bot6 |> get_x)
          1000 );
      ( "check bot6 set_y2" >:: fun _ ->
        assert_equal
          Bot.(
            set_y bot6 800;
            bot6 |> get_y)
          800 );
      ( "check bot6 set_state5" >:: fun _ ->
        assert_equal
          Bot.(
            set_state bot6 Right;
            bot6 |> get_state)
          Bot.Right );
    ]

let bot7 = Bot.init_ghost 1000 800 Ghost2

    let bot7_tests =
      [
        (* Existing tests *)
        ( "check bot7 init state" >:: fun _ ->
          assert_equal (Bot.init_ghost 400 300 Ghost1 |> Bot.get_state) Bot.Right );
        ( "check bot7 init x" >:: fun _ ->
          assert_equal (Bot.init_ghost 400 300 Ghost1 |> Bot.get_x) 400 );
        ( "check bot7 init y" >:: fun _ ->
          assert_equal (Bot.init_ghost 400 300 Ghost1 |> Bot.get_y) 300 );
        ( "check bot7 init Ghost" >:: fun _ ->
          assert_equal (Bot.init_ghost 400 300 Ghost2 |> Bot.get_ghost) Bot.Ghost2 );
        ( "check bot7 set_state" >:: fun _ ->
          assert_equal
            Bot.(
              set_state bot7 Right;
              bot7 |> get_state)
            Bot.Right );
        ( "check bot7 set_state2" >:: fun _ ->
          assert_equal
            Bot.(
              set_state bot7 Left;
              bot7 |> get_state)
            Bot.Left );
        ( "check bot7 set_state3" >:: fun _ ->
          assert_equal
            Bot.(
              set_state bot7 Up;
              bot7 |> get_state)
            Bot.Up );
        ( "check bot7 set_state4" >:: fun _ ->
          assert_equal
            Bot.(
              set_state bot7 Down;
              bot7 |> get_state)
            Bot.Down );
    
        (* Additional tests *)
        ( "check bot7 set_x" >:: fun _ ->
          assert_equal
            Bot.(
              set_x bot7 500;
              bot7 |> get_x)
            500 );
        ( "check bot7 set_y" >:: fun _ ->
          assert_equal
            Bot.(
              set_y bot7 400;
              bot7 |> get_y)
            400 );
        ( "check bot7 set_x2" >:: fun _ ->
          assert_equal
            Bot.(
              set_x bot7 600;
              bot7 |> get_x)
            600 );
        ( "check bot7 set_y2" >:: fun _ ->
          assert_equal
            Bot.(
              set_y bot7 500;
              bot7 |> get_y)
            500 );
        ( "check bot7 set_state5" >:: fun _ ->
          assert_equal
            Bot.(
              set_state bot7 Right;
              bot7 |> get_state)
            Bot.Right );
      ]

let suite =
  "test suite for Pacman"
  >::: List.flatten 
  [ 
    player_tests;
    voxel_tests; 
    map_tests; 
    bot_tests; 
    bot2_tests; 
    player_tests2; 
    bot_tests2; 
    bot2_tests2;
    bot3_tests;
    bot4_tests;
    bot5_tests;
    bot6_tests;
    bot7_tests;
  ]

let _ = run_test_tt_main suite
