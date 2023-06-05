(** Representation of game data.

    This module represents the data necessary for a game, including the ships
    and boards. It handles storing the rules of a game. *)

exception InvalidPos of (int * int)
(** Raised when attempts are made to access a position (a, b) that is not on the
    board.*)

type t
(** The abstract type of values representing the game state. *)

type ship_print = {
  name : string;
  length : int;
}
(** [ship_print] is the type of ships with only its name and length exposed. *)

type ship_expose = {
  name : string;
  s_id : int;
  length1 : int;
  length2 : int;
}
(** [ship_expose] is the type of ships with part of its information (length for
    player 1 and 2, id, and name) exposed. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] creates an instance of a game. *)

val id : t -> string
(** [id g] is the id of Game [g]. *)

val description : t -> string
(** [description t] is the description of Game [t]. *)

val ships : t -> Ship.t list
(** [ships t] is the list of ships in Game [t]. *)

val ship_names : Ship.t list -> string list
(** [ship_names t] is the list of ship names in the list of ships [t]. *)

val ship_name_length : Ship.t list -> ship_print list
(** [ship_name_length s] is the list of ships in [s] with only name and length
    exposed. *)

val ship_name_id : Ship.t list -> (string * int) list
(** [ship_name_id s] is the list of all ships in list [s] with their names and
    ids matched in a tuple. *)

val place : t -> Ship.t -> int -> int -> char -> t
(** [place t s x y o] places the ship [s] at position (x, y) in the orientation
    [o].*)

val player_turn : t -> Player.t
(** [player_turn g] returns the player whose turn it is. *)

val change_turn : t -> t
(** [change_turn game] changes the turn between two players in [game]. *)

val get_moves : t -> int
(** [get_moves g] is the number of moves recorded by g. *)

val print_board_place_7 : t -> int -> unit
(** [print_board_place_7 g p] prints the 7x7 board held by the player whose turn
    it is in Game [g] so that the player knows where they placed their ships. *)

val print_board_guess_7 : t -> int -> unit
(** [print_board_guess_7 g p] prints the 7x7 board held by the player whose turn
    it is not in Game [g] so that the player knows where they have fired. *)

val print_board_place : t -> int -> unit
(** [print_board_place g p] prints the nxn board held by the player whose turn
    it is in Game [g] so that the player knows where they placed their ships. *)

val print_board_guess : t -> int -> unit
(** [print_board_guess g p] prints the nxn board held by the player whose turn
    it is not in Game [g] so that the player knows where they have fired. *)

val place_hit : t -> int -> int -> t
(** [place_hit g x y] places a hit at the position (x, y) on the other player's
    board (i.e. not the player whose turn it is). *)

val ship_expose_list : Ship.t list -> ship_expose list
(** [ship_expose_list s] is the list of ships in [s] with only name, id, and
    length exposed. *)

val is_over : t -> bool
(** [is_over game] is true if all of one player's ships have length 0 and false
    otherwise. *)

val update_ships_sunk_list : t -> t
(** [update_ships_sunk_list game] is the game with an updated ship list with
    lengths changed to account for the previous hit. *)
