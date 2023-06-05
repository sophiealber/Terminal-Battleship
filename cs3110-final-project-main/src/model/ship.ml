type t = {
  name : string;
  id : int;
  length1 : int;
  length2 : int;
}
(* length1 is player 1's ship. when player 2 hits player 1's ship, length1 is
   decremented. *)

exception AlreadySunk of t

let init i s l = { name = s; id = i; length1 = l; length2 = l }
let get_id ship = ship.id
let get_name ship = ship.name
let is_sunk ship i = if i = 0 then ship.length1 = 0 else ship.length2 = 0
let length1 ship = ship.length1
let length2 ship = ship.length2

let decr_length ship i =
  if i = 1 then { ship with length1 = ship.length1 - 1 }
  else { ship with length2 = ship.length2 - 1 }

let rec get_unhit_length ships player =
  match ships with
  | h :: t ->
      if player = 1 then h.length2 + get_unhit_length t player
      else h.length1 + get_unhit_length t player
  | [] -> 0
