(** Representation of a player in the game.

    This module represents a player's name and their board. Each player handles
    their own board information. *)

type t
(** The abstract type of values representing a Player. *)

val init : string -> int -> t
(** [init str board_size ] is a new player with an empty board of size nxn where
    n = board_size and name [s]. *)

val get_board : t -> Board.t
(** [get_board player] gets the board of [player] *)

val place_ship : t -> Ship.t -> int -> int -> char -> t
(** [place_ship board ship] will place [ship] on board *)

val hit : t -> int -> int -> t
(** [hit p x y] places a hit at (x, y) in player [p]'s board. *)

val update_player_board : t -> int list -> t
(** [update_player_board p ship_ids] is player [p] with a new board such that
    every position that contains a ship denoted in [ship_ids] is updated to be
    sunk.*)
