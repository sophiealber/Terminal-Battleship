(* Test Plan: board.ml, boardarray.ml, game.ml, player.ml, position.ml, and
   ship.ml were tested via OUnit whereas command.ml was mainly manually tested
   via make play and trying out various edge cases. Our test cases focus on
   initialization of games and game transitions. Test cases were developed using
   a mix of glass box and black box development with an emphasis on glass box.
   By testing each small building block of a game, such as moves, position
   changes, board transitions, and turn switches, in conjunction with our
   extensive manual testing, we believe that we have tested the most reasonable
   ways our game can be played as well as typical edge cases that could occur,
   which demonstrates that our system is correct when used for its intended
   purpose.*)

open OUnit2
open Battleship

(* ________________________Position tests__________________________________*)

(* Position.get_tile testing helper function. *)
let position_tile_test (name : string) (expected : int * int) (p : Position.t) =
  name >:: fun _ -> assert_equal expected (p |> Position.get_tile)

(* Position.get_ship testing helper function. *)
let position_ship_test (name : string) (expected : int) (p : Position.t) =
  name >:: fun _ -> assert_equal expected (Position.get_ship p)

(* Position.change_hit testing helper function when a valid position is passed
   in. *)
let position_changed_h_test (name : string) (p : Position.t) =
  name >:: fun _ ->
  let changed =
    Position.get_ship p = (p |> Position.change_hit |> Position.get_ship)
    && Position.get_tile p = (p |> Position.change_hit |> Position.get_tile)
    && Position.is_hit p <> (p |> Position.change_hit |> Position.is_hit)
  in
  assert changed

(* Position.change_hit testing helper function when an invalid position is
   passed in. *)
let position_exn_changed_h_test (name : string) (p : Position.t) =
  name >:: fun _ ->
  assert_raises
    (Position.AlreadyHit (Position.get_tile p))
    (fun () -> Position.change_hit p)

(* Position.place_ship testing helper function when initial tile is
   unoccupied. *)
let position_placed_test (name : string) (p : Position.t) (s : Ship.t) =
  name >:: fun _ ->
  let placed =
    Position.get_tile p = (p |> Position.place_ship s |> Position.get_tile)
    && Position.is_hit p = (p |> Position.place_ship s |> Position.is_hit)
    && p |> Position.place_ship s |> Position.get_ship = Ship.get_id s
  in
  assert placed

let string_print s = s

let position_tostring_test (name : string) (actual : string) (expected : string)
    =
  name >:: fun _ -> assert_equal actual expected ~printer:string_print

let position_1 =
  Position.place_ship
    (Ship.init 3 "submarine" 2)
    (Position.init_position (6, 6))

let position_2 =
  Position.place_ship
    (Ship.init 4 "battleship" 4)
    (Position.init_position (2, 2))

let position_tests =
  [
    position_tile_test "get_tile (3, 3)" (3, 3) (Position.init_position (3, 3));
    position_tile_test "get_tile (3, 3)" (0, 8) (Position.init_position (0, 8));
    position_ship_test "get_ship on initialized Position" 0
      (Position.init_position (0, 9));
    position_changed_h_test "valid position (4, 6) is changed"
      ((4, 6) |> Position.init_position);
    position_changed_h_test "valid position (5, 2) is changed"
      ((5, 2) |> Position.init_position);
    position_exn_changed_h_test "invalid position (3, 2) raises AlreadyHit"
      ((3, 2) |> Position.init_position |> Position.change_hit);
    position_placed_test "ship 3 placed on (6, 6)"
      (Position.init_position (6, 6))
      (Ship.init 3 "sub" 2);
    position_tostring_test "ship 3 on position (6, 6) not hit"
      (Position.to_string position_1)
      "3";
    position_tostring_test "ship 3 on position (6, 6) not hit"
      (Position.to_string_guess position_1)
      "~";
    position_tostring_test "ship 3 on position (6, 6) hit"
      (Position.to_string (Position.change_hit position_1))
      "X";
    position_tostring_test "ship 3 on position (6, 6) hit"
      (Position.to_string_guess (Position.change_hit position_1))
      "X";
    position_tostring_test "ship 3 on position (6, 6) sunk"
      (Position.to_string
         (Position.change_sunk (Position.change_hit position_1)))
      "X";
    position_tostring_test "ship 3 on position (6, 6) sunk"
      (Position.to_string_guess
         (Position.change_sunk (Position.change_hit position_1)))
      "3";
    position_tostring_test "position (6, 6) with no ship"
      (Position.to_string (Position.init_position (6, 6)))
      "~";
    position_tostring_test "position (6, 6) with no ship"
      (Position.to_string_guess (Position.init_position (6, 6)))
      "~";
    position_tostring_test "position (6, 6) hit with no ship"
      (Position.to_string (Position.change_hit (Position.init_position (6, 6))))
      " ";
    position_tostring_test "position (6, 6) hit with no ship"
      (Position.to_string_guess
         (Position.change_hit (Position.init_position (6, 6))))
      " ";
    position_tostring_test "ship 4 on position (2, 2) not hit"
      (Position.to_string position_2)
      "4";
    position_tostring_test "ship 4 on position (2, 2) not hit"
      (Position.to_string_guess position_2)
      "~";
    position_tostring_test "ship 4 on position (2, 2) hit"
      (Position.to_string (Position.change_hit position_2))
      "X";
    position_tostring_test "ship 4 on position (2, 2) hit"
      (Position.to_string_guess (Position.change_hit position_2))
      "X";
  ]

(* ------------------------------Ship tests-------------------------------*)
let sub1 = Ship.init 1 "Submarine" 4

let decr_tests (name : string) (expected : int) (ship : Ship.t) (ship_num : int)
    =
  name >:: fun _ ->
  assert_equal
    (if ship_num = 1 then Ship.length1 (Ship.decr_length ship 1)
    else Ship.length2 (Ship.decr_length ship 2))
    expected

let ship_tests =
  [
    ( "name of sub1 is Submarine" >:: fun _ ->
      assert_equal "Submarine" (Ship.get_name sub1) );
    ("id of sub1 is 1" >:: fun _ -> assert_equal 1 (Ship.get_id sub1));
    ( "sub1 on player 1's board is initially not sunk" >:: fun _ ->
      assert_equal false (Ship.is_sunk sub1 1) );
    ( "sub2 on player 2's board is initially not sunk" >:: fun _ ->
      assert_equal false (Ship.is_sunk sub1 2) );
    decr_tests "decrementing sub1's length1 should be 3" 3 sub1 1;
    decr_tests "decrementing sub1's length2 should be 3" 3 sub1 2;
    ( "decrementing sub1's length1 should not change length2" >:: fun _ ->
      assert_equal 4 (Ship.length2 (Ship.decr_length sub1 1)) );
    ( "decrementing sub1's length2 should not change length1" >:: fun _ ->
      assert_equal 4 (Ship.length1 (Ship.decr_length sub1 2)) );
  ]

(*-----------------------Board Tests-----------------------------------*)
let ship1 = Ship.init 1 "Submarine" 3
let ship2 = Ship.init 2 "Destroyer" 2
let empty_board = Board.init_board_7
let placed_board = Board.place_ship empty_board ship1 1 3 'H'
let placed_board2 = Board.place_ship empty_board ship1 3 3 'V'
let placed_board3 = Board.place_ship placed_board2 ship2 4 4 'V'
let placed_board4 = Board.place_ship placed_board3 ship1 1 2 'H'
let placed_board5 = Board.place_ship placed_board4 ship2 6 7 'V'
let hit_board1 = Board.hit placed_board5 1 2
let hit_board2 = Board.hit hit_board1 1 3
let hit_board3 = Board.hit hit_board2 1 4
let sunk_board3 = Board.update_sunk hit_board3 [ 1 ]
let list_board_5 = Board.init 5
let list_board_7 = Board.init 7
let list_board_9 = Board.init 9
let placed_list_board_7_1 = Board.place_ship list_board_7 ship1 1 3 'H'
let placed_list_board_7_2 = Board.place_ship list_board_7 ship1 3 3 'V'
let placed_list_board_7_3 = Board.place_ship placed_list_board_7_2 ship2 4 4 'V'
let placed_list_board_7_4 = Board.place_ship placed_list_board_7_3 ship1 1 2 'H'
let placed_list_board_7_5 = Board.place_ship placed_list_board_7_4 ship2 6 7 'V'
let hit_list_board_7_1 = Board.hit placed_list_board_7_5 1 2
let hit_list_board_7_2 = Board.hit hit_list_board_7_1 1 3
let hit_list_board_7_3 = Board.hit hit_list_board_7_2 1 4
let sunk_list_board_7_3 = Board.update_sunk hit_list_board_7_3 [ 1 ]

(* Array boards below. *)
let arr_board_5 = Boardarray.init 5
let arr_board_7 = Boardarray.init 7
let arr_board_9 = Boardarray.init 9
let arr_board_7_1 = Boardarray.init 7
let arr_board_7_2 = Boardarray.init 7
let placed_arr_board_7_1 = Boardarray.place_ship arr_board_7_1 ship1 1 3 'H'
let placed_arr_board_7_2 = Boardarray.place_ship arr_board_7_2 ship1 3 3 'V'

let placed_arr_board_7_3 =
  Boardarray.place_ship placed_arr_board_7_2 ship2 4 4 'V'

let placed_arr_board_7_4 =
  Boardarray.place_ship placed_arr_board_7_3 ship1 1 2 'H'

let placed_arr_board_7_5 =
  Boardarray.place_ship placed_arr_board_7_4 ship2 6 7 'V'

let hit_arr_board_7_1 = Boardarray.hit placed_arr_board_7_5 1 2
let hit_arr_board_7_2 = Boardarray.hit hit_arr_board_7_1 1 3
let hit_arr_board_7_3 = Boardarray.hit hit_arr_board_7_2 1 4
let sunk_arr_board_7_3 = Boardarray.update_sunk hit_arr_board_7_3 [ 1 ]

let place_tests (name : string) (expected : int) (board : Board.t) (x : int)
    (y : int) =
  name >:: fun _ ->
  assert_equal expected
    (Board.get_position board x y |> Position.get_ship)
    ~printer:string_of_int

let to_string_test (name : string) board (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.to_string board true) ~printer:Fun.id

let hit_tests (name : string) (expected : bool) (board : Board.t) (x : int)
    (y : int) =
  name >:: fun _ ->
  assert_equal expected (Board.get_position board x y |> Position.is_hit)

let sunk_tests (name : string) (expected : bool) (board : Board.t) (x : int)
    (y : int) =
  name >:: fun _ ->
  assert_equal expected (Board.get_position board x y |> Position.is_sunk)

(*Below are duplicate of the above tests, we should consider making a functor
  for board and board array.*)

let place_arr_tests (name : string) (expected : int) (board : Boardarray.t)
    (x : int) (y : int) =
  name >:: fun _ ->
  assert_equal expected
    (Boardarray.get_position board x y |> Position.get_ship)
    ~printer:string_of_int

let hit_arr_tests (name : string) (expected : bool) (board : Boardarray.t)
    (x : int) (y : int) =
  name >:: fun _ ->
  assert_equal expected
    (Boardarray.get_position board x y |> Position.is_hit)
    ~printer:Bool.to_string

let sunk_arr_tests (name : string) (expected : bool) (board : Boardarray.t)
    (x : int) (y : int) =
  name >:: fun _ ->
  assert_equal expected (Boardarray.get_position board x y |> Position.is_sunk)

let board_tests =
  [
    place_tests "ship at position (1, 3) should be 1" 1 placed_board 1 3;
    ("print board" >:: fun _ -> Board.print empty_board true);
    place_tests "ship at position (1, 4) should be 1 for placed_board" 1
      placed_board 1 4;
    place_tests "ship at position (1, 5) should be 1 for placed_board" 1
      placed_board 1 5;
    place_tests "ship at position (3, 3) should be 1 for placed_board5" 1
      placed_board5 3 3;
    place_tests "ship at position (4, 3) should be 1 for placed_board5" 1
      placed_board5 4 3;
    place_tests "ship at position (5, 3) should be 1 for placed_board5" 1
      placed_board5 5 3;
    place_tests "ship at position (4, 4) should be 2 for placed_board5" 2
      placed_board5 4 4;
    place_tests "ship at position (5, 4) should be 2 for placed_board5" 2
      placed_board5 5 4;
    place_tests "ship at position (1, 2) should be 1 for placed_board5" 1
      placed_board5 1 2;
    place_tests "ship at position (1, 4) should be 1 for placed_board5" 1
      placed_board5 1 4;
    (*The following are to check placement correctness with boards of varying
      size. *)
    ( "print board" >:: fun _ ->
      print_endline (Board.to_string placed_list_board_7_1 true) );
    place_tests "ship at position (1, 3) should be 1" 1 placed_board5 1 3;
    place_tests
      "ship at position (1, 3) should be 1 for varying list board \
       placed_list_board_5_1"
      1 placed_list_board_7_1 1 3;
    place_tests
      "ship at position (1, 4) should be 1 for varying list board \
       placed_list_board_5_1"
      1 placed_list_board_7_1 1 4;
    place_tests
      "ship at position (1, 5) should be 1 for varying list board \
       placed_list_board_5_1"
      1 placed_list_board_7_1 1 5;
    hit_tests "ship at position (1, 2) should be hit for hit_board1" true
      hit_board1 1 2;
    hit_tests "ship at position (1, 3) should not be hit for hit_board1" false
      hit_board1 1 3;
    hit_tests "ship at position (1, 4) should not be hit for hit_board1" false
      hit_board1 1 4;
    hit_tests "ship at position (1, 4) should be hit for hit_board2" true
      hit_board2 1 3;
    hit_tests "ship at position (1, 5) should not be hit for hit_board3" false
      hit_board3 1 5;
    (* The following are to check hitting correctness with boards of varying
       size. *)
    hit_tests "ship at position (1, 2) should be hit" true hit_list_board_7_1 1
      2;
    hit_tests "ship at position (1, 3) should not be hit" false
      hit_list_board_7_1 1 3;
    hit_tests "ship at position (1, 4) should not be hit" false
      hit_list_board_7_1 1 4;
    hit_tests "ship at position (1, 4) should be hit" true hit_list_board_7_2 1
      3;
    hit_tests "ship at position (1, 5) should not be hit" false
      hit_list_board_7_3 1 5;
    sunk_tests "ship at position (1, 2) should be sunk for sunk_board3" true
      sunk_board3 1 2;
    sunk_tests "ship at position (1, 3) should be sunk for sunk_board3" true
      sunk_board3 1 3;
    sunk_tests "ship at position (1, 4) should be sunk for sunk_board3" true
      sunk_board3 1 4;
    (* The following are to check sinking correctness with boards of varying
       size. *)
    sunk_tests "ship at position (1, 2) should be sunk" true sunk_list_board_7_3
      1 2;
    sunk_tests "ship at position (1, 3) should be sunk" true sunk_list_board_7_3
      1 3;
    sunk_tests "ship at position (1, 4) should be sunk" true sunk_list_board_7_3
      1 4;
    (* Below are to_string tests. *)
    to_string_test "string of varying compare to init_7" list_board_7
      (Board.to_string empty_board true);
    to_string_test "string of varying list" list_board_7
      "YOUR Board:\n\
      \  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |  \n\
       ---------------------------------\n\
       A | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n\
       B | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n\
       C | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n\
       D | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n\
       E | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n\
       F | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n\
       G | ~ | ~ | ~ | ~ | ~ | ~ | ~ |    \n\
       ---------------------------------\n";
    to_string_test "compare list and array to_string size 7" list_board_7
      (Boardarray.to_string arr_board_7 true);
    to_string_test "compare list and array to_string size 9" list_board_9
      (Boardarray.to_string arr_board_9 true);
    to_string_test "list string for minimode" list_board_5
      "YOUR Board:\n\
      \  | 1 | 2 | 3 | 4 | 5 |  \n\
       -------------------------\n\
       A | ~ | ~ | ~ | ~ | ~ |    \n\
       -------------------------\n\
       B | ~ | ~ | ~ | ~ | ~ |    \n\
       -------------------------\n\
       C | ~ | ~ | ~ | ~ | ~ |    \n\
       -------------------------\n\
       D | ~ | ~ | ~ | ~ | ~ |    \n\
       -------------------------\n\
       E | ~ | ~ | ~ | ~ | ~ |    \n\
       -------------------------\n";
    (* Below are placement correctness checks for Boardarray. *)
    ( "print board" >:: fun _ ->
      print_endline (Boardarray.to_string placed_arr_board_7_1 true) );
    place_arr_tests "ARRAY: ship at position (1, 3) should be 1" 1
      placed_arr_board_7_1 1 3;
    place_arr_tests "ARRAY: ship at position (1, 4) should be 1" 1
      placed_arr_board_7_1 1 4;
    place_arr_tests "ARRAY: ship at position (1, 5) should be 1 " 1
      placed_arr_board_7_1 1 5;
    (* Below are hitting correctness checks for Boardarray. *)
    hit_arr_tests "ARRAY: ship at position (1, 2) should be hit" true
      hit_arr_board_7_1 1 2;
    hit_arr_tests "ARRAY: ship at position (1, 3) should be hit" true
      hit_arr_board_7_1 1 3;
    hit_arr_tests "ARRAY: ship at position (1, 4) should be hit" true
      hit_arr_board_7_1 1 4;
    hit_arr_tests "ARRAY: ship at position (1, 5) should not be hit" false
      hit_arr_board_7_3 1 5;
    (* Below are sinking correctness checks for Boardarray. *)
    sunk_arr_tests "ARRAY: ship at position (1, 2) should be sunk" true
      sunk_arr_board_7_3 1 2;
    sunk_arr_tests "ARRAY: ship at position (1, 3) should be sunk" true
      sunk_arr_board_7_3 1 3;
    sunk_arr_tests "ARRAY: ship at position (1, 4) should be sunk" true
      sunk_arr_board_7_3 1 4;
  ]

(*-----------------------------Player Tests-------------------------------*)
let player_tests = []

(* ------------------------------Game tests------------------------------ *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let c_game = Yojson.Basic.from_file (data_dir_prefix ^ "classic.json")
let classic_game = Game.from_json c_game
let comp_ship ship1 ship2 = compare (Ship.get_name ship1) (Ship.get_name ship2)

(* compares set-like-lists of (actual) ships *)
let cmp_set_list lst1 lst2 =
  let lst1_uniq = List.sort_uniq comp_ship lst1 in
  let lst2_uniq = List.sort_uniq comp_ship lst2 in
  List.length lst1 = List.length lst2
  && List.length lst1 = List.length lst1_uniq
  && List.length lst2 = List.length lst2_uniq
  && lst1_uniq = lst2_uniq

(* compares ship names to each other *)
let rec compare_ships_set (ships : string list) (expected : string list) =
  let ships_uniq = List.sort_uniq compare ships in
  let exp_uniq = List.sort_uniq compare expected in
  List.length ships = List.length expected
  && List.length ships = List.length ships_uniq
  && List.length expected = List.length exp_uniq
  && ships_uniq = exp_uniq

let is_over_tests (name : string) (expected : bool) (game : Game.t) =
  name >:: fun _ -> assert_equal (Game.is_over game) expected

let game_tests =
  [
    ( "id of classic_game is classic" >:: fun _ ->
      assert_equal "classic" (Game.id classic_game) );
    ( "parsed ships in classic_game correctly" >:: fun _ ->
      assert (
        compare_ships_set
          (classic_game |> Game.ships |> Game.ship_names)
          [ "Carrier"; "Battleship"; "Cruiser"; "Submarine"; "Destroyer" ]) );
    ( "description of classic_game is Classic Battleship!" >:: fun _ ->
      assert_equal "Classic Battleship!" (Game.description classic_game) );
    is_over_tests "classic is not over" false classic_game;
    ( "game moves are 0 initially" >:: fun _ ->
      assert_equal (Game.get_moves classic_game) 0 );
  ]

(*-----------------------Command Tests-----------------------------------*)
let valid_quit_test (name : string) (command : string) =
  name >:: fun _ ->
  assert_equal
    (match Command.parse command with
    | Quit -> true
    | _ -> false)
    true

let invalid_command_test (name : string) (command : string) =
  name >:: fun _ ->
  assert_equal
    (try
       match Command.parse command with
       | _ -> false
     with Command.Invalid -> true)
    true

let command_tests =
  [
    valid_quit_test "quit string is parsed into a Quit command" "quit";
    valid_quit_test
      "quit string with extra spaces is parsed into a Quit command" "   quit   ";
    ( "Quit string with extra spaces is invalid because it should be all \
       lowercase"
    >:: fun _ ->
      assert_equal
        (try
           match Command.parse "  Quit  " with
           | Quit -> false
           | _ -> false
         with Command.Invalid -> true)
        true );
    ( "hit A 1 is parsed into a Hit command with 1 1 as its location"
    >:: fun _ ->
      assert_equal
        (match Command.parse "hit A 1" with
        | Hit (1, 1) -> true
        | _ -> false)
        true );
    invalid_command_test
      "hit 1 1 is parsed is an invalid command because the first part of a \
       location is a character not an integer"
      "hit 1 1";
    invalid_command_test
      "hit 1 Z is parsed is an invalid command because the second value of the \
       location is a number"
      "hit 1 Z";
    invalid_command_test
      "hit A Z is parsed is an invalid command because both values are \
       characters when the second should be an integer"
      "hit A Z";
    invalid_command_test
      "hit A1 is parsed is an invalid command because the two arguments should \
       be seperated with a space"
      "hit A1";
    invalid_command_test
      "hit AA 11 is parsed is an invalid command because the two arguments \
       should each only be of length 1"
      "hit AA 11";
    invalid_command_test
      "hit a 1 is parsed is an invalid command because the first character \
       should be a capital letter"
      "hit a 1";
  ]

(* ---------------------------- END TESTS ----------------------------- *)

let suite =
  "test suite for A2"
  >::: List.flatten
         [ position_tests; game_tests; ship_tests; board_tests; command_tests ]

let _ = run_test_tt_main suite
