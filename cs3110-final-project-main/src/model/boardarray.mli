(** Representation of a battleship board in array form.

    This module represents the data necessary for a board, including the list of
    positions. It handles printing and updating the board. *)

type t
(** The type of values representing boards implemented with arrays. *)

val init_board_7 : t
(** [init_board] creates an empty 7 by 7 board *)

val init : int -> t
(** [init size] creates an empty [size] by [size] board. *)

val to_string : t -> bool -> string
(** [to_string board show] is the opponent string representation of [board] if
    not [show] and full representation of [board] if [show].*)

val print : t -> bool -> unit
(** [print board show] prints [board] with all the specifications/information if
    [show] and with only ~ markers if not [show].*)

val place_ship : t -> Ship.t -> int -> int -> char -> t
(** [place_ship board ship x y orientation] adds a ship to the board t and
    returns the new board. Raises [OutOfBounds] if not Horizontal or Vertical.*)

val board_print_7 : t -> bool -> unit
(** [board_print] prints to the terminal a representation of a board object
    which is 7 by 7. Raises [OutOfBounds]*)

val get_position : t -> int -> int -> Position.t
(** [get_position board x y ] returns the position at row x and column y in
    board*)

val hit : t -> int -> int -> t
(** [hit b x y] places a hit at the position (x, y) on board [b]. Raises
    [OutOfBounds].*)

val update_sunk : t -> int list -> t
(** [update_sunk b s] updates the positions on board [b] which hold ships with
    ids in [s] to be sunk.*)

exception OutOfBounds
(** Raised when a position outisde the board is referenced or a position is
    incorrectly accessed.*)
