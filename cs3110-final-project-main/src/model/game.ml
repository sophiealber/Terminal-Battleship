exception InvalidPos of (int * int)

type t = {
  id : string;
  description : string;
  board_size : int;
  ships : Ship.t list;
  players : Player.t list;
  turn : int;
  moves : int;
}

type ship_print = {
  name : string;
  length : int;
}

type ship_expose = {
  name : string;
  s_id : int;
  length1 : int;
  length2 : int;
}

let id game = game.id
let description game = game.description

let ship_of_json j : Ship.t =
  let open Yojson.Basic.Util in
  Ship.init
    (j |> member "id" |> to_int)
    (j |> member "name" |> to_string)
    (j |> member "length" |> to_int)

let from_json json =
  Yojson.Basic.Util.
    {
      id = json |> member "id" |> to_string;
      description = json |> member "description" |> to_string;
      board_size = json |> member "board size" |> to_int;
      ships = json |> member "ships" |> to_list |> List.map ship_of_json;
      players =
        [
          Player.init "Player 1" (json |> member "board size" |> to_int);
          Player.init "Player 2" (json |> member "board size" |> to_int);
        ];
      turn = 1;
      moves = 0;
    }

let get_board_size game = game.board_size
let ships (game : t) = game.ships

let rec ship_names s_list =
  match s_list with
  | [] -> []
  | h :: t -> Ship.get_name h :: ship_names t

let rec ship_name_length s_list =
  match s_list with
  | [] -> []
  | h :: t ->
      { name = Ship.get_name h; length = Ship.length1 h } :: ship_name_length t

let turn game = game.turn
let player_turn game = List.nth game.players (game.turn - 1)

let place game ship x y o =
  let new_players =
    if game.turn = 1 then
      [
        Player.place_ship (player_turn game) ship x y o; List.nth game.players 1;
      ]
    else
      [
        List.nth game.players 0; Player.place_ship (player_turn game) ship x y o;
      ]
  in
  { game with players = new_players }

let change_turn game =
  let new_turn = if game.turn = 1 then 2 else 1 in
  { game with turn = new_turn }

let get_moves game = game.moves

let print_board_place game t =
  let player = List.nth game.players (t - 1) in
  let board = Player.get_board player in
  Board.print board true

let print_board_guess game t =
  let player = List.nth game.players (2 - t) in
  let board = Player.get_board player in
  Board.print board false

let print_board_place_7 game t =
  let player = List.nth game.players (t - 1) in
  let board = Player.get_board player in
  Board.board_print_7 board true

let print_board_guess_7 game t =
  let player = List.nth game.players (2 - t) in
  let board = Player.get_board player in
  Board.board_print_7 board false

let rec ship_name_id ships =
  match ships with
  | [] -> []
  | h :: t -> (Ship.get_name h, Ship.get_id h) :: ship_name_id t

let rec find_ship ships id =
  match ships with
  | [] -> raise Not_found
  | h :: t -> if Ship.get_id h = id then h else find_ship t id

(* Returns new ship list with updated ship. *)
let rec replace_ship ships acc new_ship id =
  match ships with
  | [] -> []
  | h :: t ->
      if Ship.get_id h = id then (new_ship :: acc) @ t
      else replace_ship t (h :: acc) new_ship id

(** [update_ship game x y] decrements the length of the ship stored at (x, y)
    because the ship has been hit and returns the new list of ships. *)
let update_ship game x y =
  let new_ship_id =
    Board.get_position
      ((if game.turn = 1 then List.nth game.players 1
       else List.nth game.players 0)
      |> Player.get_board)
      x y
    |> Position.get_ship
  in
  if new_ship_id = 0 then game.ships
  else
    let new_ship = find_ship game.ships new_ship_id in
    replace_ship game.ships []
      (if game.turn = 1 then Ship.decr_length new_ship 2
      else Ship.decr_length new_ship 1)
      new_ship_id

let place_hit game x y =
  let new_player =
    Player.hit
      (if game.turn = 1 then List.nth game.players 1
      else List.nth game.players 0)
      x y
  in
  if game.turn = 1 then
    {
      game with
      players = [ List.nth game.players 0; new_player ];
      ships = update_ship game x y;
      moves = game.moves + 1;
    }
  else
    {
      game with
      players = [ new_player; List.nth game.players 1 ];
      ships = update_ship game x y;
      moves = game.moves + 1;
    }

let rec ship_expose_list ships =
  match ships with
  | [] -> []
  | h :: t ->
      {
        name = Ship.get_name h;
        s_id = Ship.get_id h;
        length1 = Ship.length1 h;
        length2 = Ship.length2 h;
      }
      :: ship_expose_list t

let is_over game =
  List.fold_left
    (fun acc elt -> elt.length1 = 0 && acc)
    true
    (ship_expose_list game.ships)
  || List.fold_left
       (fun acc elt -> elt.length2 = 0 && acc)
       true
       (ship_expose_list game.ships)

let update_ships_sunk_list game =
  let rec get_sunk_ships ships =
    (* getting the ids of the sunk ships*)
    match ships with
    | [] -> []
    | h :: t ->
        if game.turn = 1 then
          if Ship.length2 h = 0 then Ship.get_id h :: get_sunk_ships t
          else get_sunk_ships t
        else if Ship.length1 h = 0 then Ship.get_id h :: get_sunk_ships t
        else get_sunk_ships t
  in
  let sunk_ships = get_sunk_ships game.ships in
  let new_player =
    if game.turn = 1 (* changing player 2's board *) then
      Player.update_player_board (List.nth game.players 1) sunk_ships
    else Player.update_player_board (List.nth game.players 0) sunk_ships
  in
  if game.turn = 1 then
    (* returning a new game *)
    { game with players = [ List.nth game.players 0; new_player ] }
  else { game with players = [ new_player; List.nth game.players 1 ] }
