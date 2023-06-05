type t = Position.t array array

exception OutOfBounds

let init_board_7 =
  let og_array = Array.make_matrix 7 7 (Position.init_position (0, 0)) in
  for r = 0 to 6 do
    for c = 0 to 6 do
      og_array.(r).(c) <- Position.init_position (r, c)
    done
  done;
  og_array

let size (board : t) = Array.length board

let init x =
  let og_array = Array.make_matrix x x (Position.init_position (0, 0)) in
  for r = 0 to x - 1 do
    for c = 0 to x - 1 do
      og_array.(r).(c) <- Position.init_position (r, c)
    done
  done;
  og_array

let pad_to_three s =
  if String.length s == 1 then " " ^ s ^ " "
  else if String.length s == 2 then " " ^ s
  else s

let num_bit board =
  let rec nummy acc counter =
    match counter with
    | 0 -> "  |" ^ acc
    | _ ->
        nummy ((pad_to_three (string_of_int counter) ^ "|") ^ acc) (counter - 1)
  in
  nummy "  \n" (size board)

let dash_string board = String.make ((size board * 4) + 5) '-' ^ "\n"

let header_string board show =
  if show then String.cat ("YOUR Board:\n" ^ num_bit board) (dash_string board)
  else
    String.cat ("Your OPPONENTS's Board:\n" ^ num_bit board) (dash_string board)

let entry_string pos show =
  if show || Position.is_sunk pos then pad_to_three (Position.to_string pos)
  else pad_to_three (Position.to_string_guess pos)

let entry_string_tru pos = entry_string pos true
let entry_string_fals pos = entry_string pos false

let row_string row show =
  if show then
    " |"
    ^ String.concat "|" (Array.to_list (Array.map entry_string_tru row))
    ^ "|    " ^ "\n"
  else
    " |"
    ^ String.concat "|" (Array.to_list (Array.map entry_string_fals row))
    ^ "|    " ^ "\n"

let row_string_tru row = row_string row true
let row_string_fals row = row_string row false

let board_string (board : t) show =
  let rec aux board' index acc =
    if show then
      match board' with
      | [] -> acc
      | h :: t ->
          aux t (index + 1)
            (acc
            ^ Char.escaped (Char.chr ((index mod 256) + 65))
            ^ row_string_tru h ^ dash_string board)
    else
      match board' with
      | [] -> acc
      | h :: t ->
          aux t (index + 1)
            (acc
            ^ Char.escaped (Char.chr ((index mod 256) + 65))
            ^ row_string_fals h ^ dash_string board)
  in
  aux (Array.to_list board) 0 ""

let to_string board show =
  if show then header_string board true ^ board_string board true
  else header_string board false ^ board_string board false

let print board show = print_endline (to_string board show)

let place_ship (board : t) (ship : Ship.t) (x : int) (y : int)
    (orientation : char) =
  let r_x = x - 1 in
  let r_y = y - 1 in
  let size = Array.length board - 1 in
  (* first check if initial position is valid *)
  if r_x < 0 || r_y < 0 || r_x > size || r_y > size then raise OutOfBounds
  else
    let ship_len = Ship.length1 ship in
    match orientation with
    | 'H' ->
        if r_y + ship_len - 1 > size then raise OutOfBounds
        else
          for i = r_y to r_y + ship_len - 1 do
            board.(r_x).(i) <- Position.place_ship ship board.(r_x).(i)
          done;
        board
    | 'V' ->
        if r_x + ship_len - 1 > size then raise OutOfBounds
        else
          for i = r_x to r_x + ship_len - 1 do
            board.(i).(r_y) <- Position.place_ship ship board.(i).(r_y)
          done;
        board
    | _ -> raise OutOfBounds

let board_print_7 (board : t) show =
  print_endline "\n\n";
  print_endline "  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |  ";
  let dashstring = "---------------------------------" in
  let alphabet =
    [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N" ]
  in
  let rec loop_print i board board_length =
    if i < board_length then
      match board with
      | [] -> raise OutOfBounds
      | h :: t -> (
          match h with
          | h2 :: t2 ->
              print_endline dashstring;
              print_endline
                (List.nth alphabet i ^ " | "
                ^ (if show then Position.to_string (List.nth h 0)
                  else Position.to_string_guess (List.nth h 0))
                ^ " | "
                ^ (if show then Position.to_string (List.nth h 1)
                  else Position.to_string_guess (List.nth h 1))
                ^ " | "
                ^ (if show then Position.to_string (List.nth h 2)
                  else Position.to_string_guess (List.nth h 2))
                ^ " | "
                ^ (if show then Position.to_string (List.nth h 3)
                  else Position.to_string_guess (List.nth h 3))
                ^ " | "
                ^ (if show then Position.to_string (List.nth h 4)
                  else Position.to_string_guess (List.nth h 4))
                ^ " | "
                ^ (if show then Position.to_string (List.nth h 5)
                  else Position.to_string_guess (List.nth h 5))
                ^ " | "
                ^ (if show then Position.to_string (List.nth h 6)
                  else Position.to_string_guess (List.nth h 6))
                ^ " |  ");
              loop_print (i + 1) t board_length
          | [] -> print_endline "got an empty alphabet")
    else print_endline (dashstring ^ "\n\n")
  in
  loop_print 0
    (Array.to_list (Array.map Array.to_list board))
    (Array.length board)

let get_position board x y = board.(x - 1).(y - 1)

let hit board x y =
  let r_x = x - 1 in
  let r_y = y - 1 in
  if
    r_x < 0 || r_y < 0
    || r_x > Array.length board - 1
    || r_y > Array.length board - 1
  then raise OutOfBounds
  else board.(r_x).(r_y) <- Position.change_hit board.(r_x).(r_y);
  board

let update_sunk (board : t) sunk_ships =
  let board_length = Array.length board in
  if List.length sunk_ships = 0 then board
  else
    let boardy =
      let rec loop_row i board board_length =
        if i < board_length then
          match board with
          | [] -> raise OutOfBounds
          | h :: t ->
              let rec loop_col j head board_length =
                if j < board_length - 1 then
                  match head with
                  | [] -> raise OutOfBounds
                  | hd :: tl ->
                      (if List.mem (Position.get_ship hd) sunk_ships then
                       Position.change_sunk hd
                      else hd)
                      :: loop_col (j + 1) tl board_length
                else
                  let lp = List.nth head (List.length head - 1) in
                  if List.mem (Position.get_ship lp) sunk_ships then
                    [ Position.change_sunk lp ]
                  else [ lp ]
              in
              loop_col 0 h board_length :: loop_row (i + 1) t board_length
        else []
      in
      loop_row 0 (Array.to_list (Array.map Array.to_list board)) board_length
    in
    Array.of_list (List.map Array.of_list boardy)
