exception AlreadyHit of (int * int)
exception Occupied of int

type t = {
  tile : int * int;
  ship : int;
  h_status : bool;
  is_sunk : bool;
}

let init_position name =
  { tile = name; ship = 0; h_status = false; is_sunk = false }

let get_tile = function
  | { tile } -> tile

let get_ship = function
  | { ship } -> ship

let is_hit = function
  | { h_status } -> h_status

let is_sunk = function
  | { is_sunk } -> is_sunk

let change_hit p =
  match p with
  | { tile; ship; h_status = true; is_sunk } -> raise (AlreadyHit tile)
  | { tile; ship; h_status; is_sunk } ->
      { tile; ship; h_status = true; is_sunk }

let change_sunk p =
  match p with
  | { tile; ship; h_status; is_sunk } ->
      { tile; ship; h_status; is_sunk = true }

let place_ship s p =
  let id = Ship.get_id s in
  match p with
  | { tile; ship = 0; h_status; is_sunk } ->
      { tile; ship = id; h_status; is_sunk }
  | { ship } -> raise (Occupied id)

let to_string p =
  match is_hit p with
  | true -> (
      match get_ship p with
      | 0 -> " "
      | _ -> "X")
  | false -> (
      match get_ship p with
      | 0 -> "~"
      | _ -> string_of_int (get_ship p))

let to_string_guess p =
  match is_hit p with
  | true -> (
      match get_ship p with
      | 0 -> " "
      | _ -> if is_sunk p then string_of_int (get_ship p) else "X")
  | false -> "~"
