type t = Position.t list list

exception OutOfBounds

(** Boards cannot be bigger than 26*)
let default : t =
  let rec loop_init i newboard =
    if i < 8 then
      loop_init (i + 1)
        (List.append newboard
           [
             [
               Position.init_position (i, 1);
               Position.init_position (i, 2);
               Position.init_position (i, 3);
               Position.init_position (i, 4);
               Position.init_position (i, 5);
               Position.init_position (i, 6);
               Position.init_position (i, 7);
             ];
           ])
    else newboard
  in
  loop_init 1 []

let size (board : t) = List.length board

(** Classic board type*)
let init size : t =
  if size > 26 then default
  else
    let rec init_row (acc : Position.t list list) row =
      match row with
      | 0 -> acc
      | _ ->
          let rec init_entries (row_acc : Position.t list) col =
            match col with
            | 0 -> init_row (row_acc :: acc) (row - 1)
            | _ ->
                init_entries
                  (Position.init_position (row, col) :: row_acc)
                  (col - 1)
          in
          init_entries [] size
    in
    init_row [] size

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
  if show (*|| Position.is_sunk pos*) then pad_to_three (Position.to_string pos)
  else pad_to_three (Position.to_string_guess pos)

let entry_string_tru pos = entry_string pos true
let entry_string_fals pos = entry_string pos false

let row_string row show =
  if show then
    " |" ^ String.concat "|" (List.map entry_string_tru row) ^ "|    " ^ "\n"
  else
    " |" ^ String.concat "|" (List.map entry_string_fals row) ^ "|    " ^ "\n"

let row_string_tru row = row_string row true
let row_string_fals row = row_string row false

let board_stringy (board : t) show =
  if show then String.concat (dash_string board) (List.map row_string_tru board)
  else String.concat (dash_string board) (List.map row_string_fals board)

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
  aux board 0 ""

let to_string board show =
  if show then header_string board true ^ board_string board true
  else header_string board false ^ board_string board false

let print board show = print_endline (to_string board show)

let hit board x y =
  let rec hit_r board x countr =
    match board with
    | [] -> raise OutOfBounds
    | h :: t ->
        if countr = x then
          let rec hit_c nested y countc =
            match nested with
            | [] -> raise OutOfBounds
            | hd :: tl ->
                if countc = y then Position.change_hit hd :: tl
                else hd :: hit_c tl y (countc + 1)
          in
          hit_c h y 1 :: t
        else h :: hit_r t x (countr + 1)
  in
  hit_r board x 1

let get_position board x y = List.nth (List.nth board (x - 1)) (y - 1)

let place_1ship (board : t) (ship : Ship.t) (x : int) (y : int) =
  let rec replace_elem_r board x countr =
    match board with
    | [] -> raise OutOfBounds
    | h :: t ->
        if countr = x then
          let rec replace_elem_c nested y countc =
            match nested with
            | [] -> raise OutOfBounds
            | hd :: tl ->
                if countc = y then Position.place_ship ship hd :: tl
                else hd :: replace_elem_c tl y (countc + 1)
          in
          replace_elem_c h y 1 :: t
        else h :: replace_elem_r t x (countr + 1)
  in
  replace_elem_r board x 1

let place_ship (board : t) (ship : Ship.t) (x : int) (y : int)
    (orientation : char) =
  match orientation with
  | 'H' ->
      let ship_length = Ship.length1 ship + y in
      let rec iter_ship_h board y ship_length =
        if y < ship_length then
          iter_ship_h (place_1ship board ship x y) (y + 1) ship_length
        else board
      in
      iter_ship_h board y ship_length
  | 'V' ->
      let ship_length = Ship.length1 ship + x in
      let rec iter_ship_v board x ship_length =
        if x < ship_length then
          iter_ship_v (place_1ship board ship x y) (x + 1) ship_length
        else board
      in
      iter_ship_v board x ship_length
  | _ -> raise OutOfBounds

let init_board_7 =
  let rec loop_init i newboard =
    if i < 8 then
      loop_init (i + 1)
        (List.append newboard
           [
             [
               Position.init_position (i, 1);
               Position.init_position (i, 2);
               Position.init_position (i, 3);
               Position.init_position (i, 4);
               Position.init_position (i, 5);
               Position.init_position (i, 6);
               Position.init_position (i, 7);
             ];
           ])
    else newboard
  in
  loop_init 1 []

let board_print_7 board show =
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
  loop_print 0 board (List.length board)

let update_sunk board sunk_ships =
  let board_length = List.length board in
  if List.length sunk_ships = 0 then board
  else
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
    loop_row 0 board board_length
