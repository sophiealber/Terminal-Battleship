(** Representation of a position within a board.

    This module represents a specific position on a board and the information.
    Each position handles its own status. *)

exception AlreadyHit of (int * int)
(** Raised when a position has already been hit. It carries the name of the
    position. *)

exception Occupied of int
(** Raised when attempts are made to place a ship in an already occupied
    position. It carries the name of the ship that has already been placed. *)

type t
(** The abstract type of values representing a Position. *)

val init_position : int * int -> t
(** [init_position s] creates an empty, un-hit position with tile name [s]. *)

val get_tile : t -> int * int
(** [get_tile p] is the name of the tile in position [p]. *)

val get_ship : t -> int
(** [get_ship p] is the number of the ship at position [p], or 0 if no ship
    exists. *)

val is_hit : t -> bool
(** [is_hit p] is false if position [p] has not already been hit and true
    otherwise. *)

val is_sunk : t -> bool
(** [is_sunk p] is false if there is a ship at [p] and the ship is sunk. *)

val change_hit : t -> t
(** [change_hit p] is the position [p] with the updated hit status. Raises
    [AlreadyHit s] if the position with name [s] has already been hit. *)

val change_sunk : t -> t
(** [change_sunk p] is the position [p] with the updated sunk status. *)

val place_ship : Ship.t -> t -> t
(** [place_ship p s] is the position [p] with ship [s] placed on it. Raises
    [Occupied b] if there is already a ship [b] occupying position [p]. *)

val to_string : t -> string
(** [to_string p] is the printable string representation of position [p].*)

val to_string_guess : t -> string
(** [to_string_guess p] is the printable string representation of position [p].
    It differs from to_string in that if a ship has not been hit its location is
    not revealed. *)
