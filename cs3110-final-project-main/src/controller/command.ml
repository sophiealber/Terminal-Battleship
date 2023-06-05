let data_dir_prefix = "data" ^ Filename.dir_sep
let classic = Yojson.Basic.from_file (data_dir_prefix ^ "classic.json")
let mini_mode = Yojson.Basic.from_file (data_dir_prefix ^ "mini_mode.json")

exception Invalid
exception NotValidShip

type information = {
  boat_name : string;
  position : int * int;
  orientation : char;
}

type command =
  | Quit
  | Place of information
  | Hit of int * int

let get_name (ship : Game.ship_expose) =
  match ship with
  | { name; s_id; length1; length2 } -> name

let index (c : char) : int = Char.code c - 64

let int_of_char x =
  if Char.code x >= 65 && Char.code x <= 90 then Char.code x - 64
  else raise Invalid

let parse input =
  match String.split_on_char ' ' input |> List.filter (fun x -> x <> "") with
  | [ "quit" ] -> Quit
  | [ "place"; name; x_pos; y_pos; orientation ] -> (
      try
        match int_of_string y_pos with
        | _ ->
            if
              String.length x_pos <> 1
              || (orientation <> "V" && orientation <> "H")
            then raise Invalid
            else
              Place
                {
                  boat_name = name;
                  position =
                    (String.get x_pos 0 |> int_of_char, int_of_string y_pos);
                  orientation = String.get orientation 0;
                }
      with e -> raise Invalid)
  | [ "hit"; x_pos; y_pos ] -> (
      if String.length x_pos <> 1 then raise Invalid
      else
        try
          let y = int_of_string y_pos in
          let x = String.get x_pos 0 |> int_of_char in
          match (x, y) with
          | _ -> Hit (x, y)
        with e -> raise Invalid)
  | [] | _ -> raise Invalid

let print_boats (ship : Game.ship_expose) =
  match ship with
  | { name; s_id; length1 } ->
      "| " ^ name ^ " (" ^ string_of_int length1 ^ " x 1)\n"

let rec parse_mode input =
  match String.split_on_char ' ' input |> List.filter (fun x -> x <> "") with
  | [ "classic" ] -> Game.from_json classic
  | [ "mini"; "mode" ] -> Game.from_json mini_mode
  | mode -> (
      let parsed_mode = String.concat "_" mode in
      try
        match
          Yojson.Basic.from_file (data_dir_prefix ^ parsed_mode ^ ".json")
        with
        | _ ->
            Game.from_json
              (Yojson.Basic.from_file (data_dir_prefix ^ parsed_mode ^ ".json"))
      with e ->
        print_endline
          (String.concat " " mode
         ^ " is not a valid mode of Battleship. Please try again.");
        print_string "> ";
        parse_mode (read_line ()))

let hit_or_quit_message player =
  "Player " ^ string_of_int player
  ^ ": Enter the action you want to perform. You have a choice of either \
     quitting the game using the command \"quit\" or hitting a boat using the \
     command \"hit\" [position]. The position is indicated by a letter \
     followed by a number, with the two characters seperated by a space."

let place_or_quit_message player =
  "Player " ^ string_of_int player
  ^ ": Enter the action you want to perform. You have a choice of either \
     quitting the game using the command \"quit\" or placing a boat using the \
     command \"place\" [name of boat] [position] [V/H]. The position is \
     indicated by a letter followed by a number, with the two characters \
     seperated by a space. The V/H required option indicates the placement of \
     the boat, either vertically or horizontally, respectively."

let general_message player =
  "Player " ^ string_of_int player
  ^ ": Enter the action you want to perform. You have a choice of either \
     quitting the game using the command \"quit\" or placing a boat using the \
     command \"place\" [name of boat] [position] [V/H] or hitting a boat using \
     the command \"hit\" [position] (only given all other players have already \
     placed their boats). For both the \"place\" and \"hit\" commands, the \
     position is indicated by a letter followed by a number inclusive, the two \
     characters seperated by a space. The V/H required option for the \
     \"place\" command indicates the placement of the boat, either vertically \
     or horizontally, respectively."

let rec ship_from_name original_boats_list boat_name ships_list
    (ships_left : Game.ship_expose list) =
  match ships_left with
  | { name; s_id; length1; length2 } :: t ->
      if name = boat_name then
        List.nth ships_list (List.length original_boats_list - s_id)
      else ship_from_name original_boats_list boat_name ships_list t
  | _ -> raise NotValidShip

let game_place original_boats_list x y o game boat_name
    (ships_left : Game.ship_expose list) player =
  let ship =
    ship_from_name original_boats_list boat_name (Game.ships game) ships_left
  in
  Game.place game ship x y o

let boats_left boat_name boats_list =
  let boat_names = List.map (fun x -> get_name x) boats_list in
  if List.mem boat_name boat_names then
    List.filter (fun x -> get_name x <> boat_name) boats_list
  else boats_list

let print_updated_boats boats_list player =
  print_endline
    ("Player " ^ string_of_int player
   ^ ": here are your (updated) list of boats.");
  print_endline (List.fold_right ( ^ ) (List.map print_boats boats_list) "")

let calc_stats game (og_boats : Game.ship_expose list) =
  let rec get_unhit (ships : Game.ship_expose list) player =
    match ships with
    | h :: t ->
        if player = 1 then h.length2 + get_unhit t player
        else h.length1 + get_unhit t player
    | [] -> 0
  in
  (* want to get accuracy statistics: total ship hits / moves *)
  let moves = Game.get_moves game in
  let p1_moves = if moves mod 2 = 1 then (moves / 2) + 1 else moves / 2 in
  let p2_moves = moves - p1_moves in
  let p1_remain =
    if moves mod 2 = 1 then 0 else Ship.get_unhit_length (Game.ships game) 1
  in
  let p2_remain =
    if moves mod 2 = 0 then 0 else Ship.get_unhit_length (Game.ships game) 2
  in
  let og_length = get_unhit og_boats 1 in
  let p1_acc =
    int_of_float
      (float_of_int (og_length - p1_remain) /. float_of_int p1_moves *. 100.)
  in

  let p2_acc =
    int_of_float
      (float_of_int (og_length - p2_remain) /. float_of_int p2_moves *. 100.)
  in
  print_endline
    ("Player 1 took " ^ string_of_int p1_moves ^ " moves and was accurate "
    ^ string_of_int (og_length - p1_remain)
    ^ " times, for a total accuracy of " ^ string_of_int p1_acc ^ "%.");
  print_endline
    ("Player 2 took " ^ string_of_int p2_moves ^ " moves and was accurate "
    ^ string_of_int (og_length - p2_remain)
    ^ " times, for a total accuracy of " ^ string_of_int p2_acc ^ "%.")

let show_stats game og_boats =
  print_endline "";
  print_endline
    "Do you want to see your stats? Please enter yes to see them, or any other \
     key to exit the game.";
  print_string "> ";
  if
    String.lowercase_ascii (read_line ()) = "y"
    || String.lowercase_ascii (read_line ()) = "yes"
  then calc_stats game og_boats
  else print_endline "Thanks for playing!"

let rec done_switching input is_fst_player =
  match input with
  | "yes" | "Yes" -> true
  | _ ->
      if is_fst_player then
        print_endline "Please enter \"yes\" if you've switched the laptop."
      else print_endline "Please enter \"yes\" if you have the laptop.";
      print_string "> ";
      done_switching (read_line ()) is_fst_player

let rec play_game game input boats_list1 boats_list2 original_boats player =
  ignore (Sys.command "clear");
  try
    match parse input with
    | Quit -> exit 0
    | Place { boat_name; position = x, y; orientation } ->
        if List.length boats_list1 = 0 && List.length boats_list2 = 0 then (
          (* case where you can't place any more boats because both players have
             placed all of of their boats*)
          print_endline
            "The [place] command isn't a valid move right now. Both players \
             have placed all their boats. Please try again.";
          print_endline (hit_or_quit_message player);
          print_string "> ";
          play_game game (read_line ()) boats_list1 boats_list2 original_boats
            player)
        else if List.length boats_list1 <> 1 && List.length boats_list1 <> 0
        then (
          let new_game =
            game_place original_boats x y orientation game boat_name boats_list1
              player
          in
          Game.print_board_place new_game player;
          let updated_boats = boats_left boat_name boats_list1 in
          print_updated_boats updated_boats player;
          print_endline "Please enter a command.";
          print_string "> ";
          play_game new_game (read_line ()) updated_boats boats_list2
            original_boats player)
        else if List.length boats_list1 = 1 then (
          let new_game =
            game_place original_boats x y orientation game boat_name boats_list1
              player
          in
          Game.print_board_place new_game player;
          print_endline "Pass the computer.";
          print_endline
            ("Player " ^ string_of_int player
           ^ ": Are you ready to switch? Enter \"yes\" if you are.");
          print_string "> ";
          if done_switching (read_line ()) true then
            ignore (Sys.command "clear");
          print_endline
            ("Player "
            ^ string_of_int (3 - player)
            ^ ": Are you ready? Enter \"yes\" if you are.");
          print_string "> ";
          if done_switching (read_line ()) false then
            ignore (Sys.command "clear");
          print_string (place_or_quit_message 2);
          print_updated_boats original_boats 2;
          let updated_boats = boats_left boat_name boats_list1 in
          print_endline "Please enter a command.";
          print_string "> ";
          play_game
            (Game.change_turn new_game)
            (read_line ()) updated_boats boats_list2 original_boats (3 - player))
        else
          let new_game =
            game_place original_boats x y orientation game boat_name boats_list2
              player
          in
          Game.print_board_place new_game player;
          let updated_boats = boats_left boat_name boats_list2 in
          if updated_boats <> [] then (
            print_updated_boats updated_boats 2;
            print_endline "Please enter a command.";
            print_string "> ")
          else (
            print_endline "Pass the computer to the other person.";
            print_endline
              ("Player " ^ string_of_int player
             ^ ": Are you ready to switch? Enter \"yes\" if you are.");
            print_string "> ";
            if done_switching (read_line ()) true then (
              ignore (Sys.command "clear");
              print_endline
                ("Player "
                ^ string_of_int (3 - player)
                ^ ": Are you ready? Enter \"yes\" if you are.");
              print_string "> ";
              if done_switching (read_line ()) false then (
                print_endline
                  ("-----------Player "
                  ^ string_of_int (3 - player)
                  ^ "'s Boards before you hit-----------");
                Game.print_board_guess new_game (3 - player);
                Game.print_board_place new_game (3 - player);
                print_endline (hit_or_quit_message (3 - player));
                print_endline "Please enter a command.";
                print_string "> ")));
          if updated_boats <> [] then
            play_game new_game (read_line ()) boats_list1 updated_boats
              original_boats player
          else
            play_game
              (Game.change_turn new_game)
              (read_line ()) boats_list1 updated_boats original_boats
              (3 - player)
    | Hit (x, y) ->
        if List.length boats_list1 = 0 && List.length boats_list2 = 0 then (
          let new_game =
            Game.place_hit game x y |> Game.update_ships_sunk_list
          in
          print_endline
            ("-----------Player " ^ string_of_int player
           ^ "'s Boards after you hit-----------");
          Game.print_board_guess new_game player;
          Game.print_board_place new_game player;
          if Game.is_over new_game then (
            print_endline ("Player " ^ string_of_int player ^ " won!");
            show_stats new_game original_boats;
            exit 0)
          else (
            print_endline
              ("Player " ^ string_of_int player
             ^ ": Are you ready to switch? Enter \"yes\" if you are.");
            print_string "> ";
            if done_switching (read_line ()) true then (
              ignore (Sys.command "clear");
              print_endline
                ("Player "
                ^ string_of_int (3 - player)
                ^ ": Are you ready? Enter \"yes\" if you are.");
              print_string "> ";
              if done_switching (read_line ()) false then (
                ignore (Sys.command "clear");
                print_endline
                  ("-----------Player "
                  ^ string_of_int (3 - player)
                  ^ "'s Boards before you hit-----------");
                Game.print_board_guess new_game (3 - player);
                Game.print_board_place new_game (3 - player);
                print_endline (hit_or_quit_message (3 - player));
                print_string "> ";
                play_game
                  (Game.change_turn new_game)
                  (read_line ()) boats_list1 boats_list2 original_boats
                  (3 - player)))
            else (
              print_endline
                "The [hit] command isn't a valid move right now. Please try \
                 again.";
              print_endline (place_or_quit_message player);
              print_string "> ";
              play_game new_game (read_line ()) boats_list1 boats_list2
                original_boats player)))
  with
  | Invalid ->
      Game.print_board_place game player;
      print_endline "Your move was invalid. Please try again.";
      print_string "> ";
      play_game game (read_line ()) boats_list1 boats_list2 original_boats
        player
  | Board.OutOfBounds ->
      Game.print_board_place game player;
      print_endline "You referenced an invalid position. Please try again.";
      print_string "> ";
      play_game game (read_line ()) boats_list1 boats_list2 original_boats
        player
  | Position.AlreadyHit _ ->
      print_endline
        ("-----------Player " ^ string_of_int player
       ^ "'s Boards already hit-----------");
      Game.print_board_guess game player;
      Game.print_board_place game player;
      print_endline "This position was already hit. Please try again.";
      print_string "> ";
      play_game game (read_line ()) boats_list1 boats_list2 original_boats
        player
  | Position.Occupied _ ->
      Game.print_board_place game player;
      print_endline
        "This position is already being occupied by another boat. Please try \
         again.";
      print_string "> ";
      play_game game (read_line ()) boats_list1 boats_list2 original_boats
        player
  | NotValidShip ->
      Game.print_board_place game player;
      print_endline "The placement of that ship is not valid. Please try again.";
      print_string "> ";
      play_game game (read_line ()) boats_list1 boats_list2 original_boats
        player
