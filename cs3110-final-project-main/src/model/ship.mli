(** Representation of a ship on the board.

    This module represents a specific ship on a board. Each ship handles its own
    status. *)

type t
(** The abstract type of values representing a Ship. *)

exception AlreadySunk of t
(** Raised when attempts are made to sink a ship [s] when [s] is already sunk. *)

val init : int -> string -> int -> t
(** [init i s] creates a ship with id [i] and name [s]. *)

val get_name : t -> string
(** [get_name s] returns the name of ship [s]. *)

val get_id : t -> int
(** [get_id s] returns the id associated with ship [s].*)

val is_sunk : t -> int -> bool
(** [is_sunk s i] is true if ship [s] has been sunk in player i's board and
    false otherwise. *)

val length1 : t -> int
(** [length1 t] is the length of ship [t] in player 1's board. *)

val length2 : t -> int
(** [length2 t] is the length of ship [t] in player 2's board. *)

val decr_length : t -> int -> t
(** [decr_list ship i] decreases the corresponding lengthi of [ship] by 1 to
    indicate it has been hit. *)

val get_unhit_length : t list -> int -> int
(** [get_unhit_length ships p] is the number of ships player number [p] needs to
    hit to win. *)
