type t = {
  name : string;
  board : Board.t;
}

let init s board_size = { name = s; board = Board.init board_size }
let get_board player = player.board

let place_ship player ship x y orientation =
  {
    name = player.name;
    board = Board.place_ship (get_board player) ship x y orientation;
  }

let hit player x y = { player with board = Board.hit player.board x y }

let update_player_board player sunk_list =
  { player with board = Board.update_sunk player.board sunk_list }
