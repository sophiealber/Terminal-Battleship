(** Handles what users see in the terminal. *)

(** [board_print size] prints to the terminal an empty board of [size] by [size] *)
let board_print size =
  print_endline "  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |  ";
  let dashstring = "---------------------------------" in
  let letter_string = " | ~ | ~ | ~ | ~ | ~ | ~ | ~ |  " in
  let alphabet =
    [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N" ]
  in
  let rec loop_print alphabet size =
    if size > 0 then
      match alphabet with
      | h :: t ->
          print_endline dashstring;
          print_endline (h ^ letter_string);
          loop_print t (size - 1)
      | [] -> print_endline "got an empty alphabet"
    else print_endline (dashstring ^ "\n")
  in
  loop_print alphabet size

(** [print_boats s] prints the name and length of ship [s]. *)
let print_boats (ship : Game.ship_expose) =
  match ship with
  | { name; s_id; length1 } ->
      "| " ^ name ^ " (" ^ string_of_int length1 ^ " x 1)\n"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to Battleship.\n";
  print_endline
    "Please enter the name of the game mode you want to play: mini mode or \
     classic.\n";
  print_string "> ";
  let new_game = Command.parse_mode (read_line ()) in
  (* print_endline "Player 1: Here is your empty board\n\n"; board_print 7;
     print_endline "Player 2: Here is your empty board\n\n"; board_print 7; *)
  print_endline
    "Here are the list of boats you have (left) to place (listed as [boat  \
     name] [size of boat as n x m where n represents its length]).";
  let ships = Game.ships new_game |> Game.ship_expose_list in
  print_endline (List.fold_right ( ^ ) (List.map print_boats ships) "");
  print_endline
    "Player 1: Enter the action you want to perform. You have a choice of \
     either quitting the game using the command \"quit\" or placing a boat \
     using the command \"place\" [name of boat] [position] [V/H]. The position \
     is indicated by a letter from followed by a number, with the two \
     characters seperated by a space. The V/H  required option indicates the \
     placement of the boat, either vertically or horizontally, respectively.";
  print_string "> ";
  Command.play_game new_game (read_line ()) ships ships ships 1

let () = main ()
