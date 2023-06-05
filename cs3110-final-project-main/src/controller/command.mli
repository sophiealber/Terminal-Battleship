(** This module parses and interacts with players' commands. *)

exception Invalid
(** Raised when an invalid command is parsed for errors involving entering a
    command with an invalid number of arguments or incorrect argument types *)

exception NotValidShip
(** Raised when an invalid ship is attempted to be utilized. *)

type information = {
  boat_name : string;
  position : int * int;
  orientation : char;
}
(** The type [information] represents the data held by the [Place] command.
    Invariant: None of these fields should be empty. *)

(** The type [command] represents a player command that is decomposed.
    Invariant: the [information] carried by [Place] and the tuple of the
    location carried by [Hit] must not be empty. *)
type command =
  | Quit
  | Place of information
  | Hit of int * int

val parse_mode : string -> Game.t
(** [parse_mode input] parses a player's input (as a json file) into a [Game]
    which contains information such as the different boats for each players, the
    number of players, and the lengths of the boats; prompts the user to
    continuously enter in a valid mode type with "classic" being the only mode
    currently implemented *)

val parse : string -> command
(** [parse input] parses a player's input into either a [Place of information],
    [Quit], or [Hit] command depending on the first word of the user's string
    input and on the number of arguments the user provides. For
    [Place of information], the input has to start with the word, "place" and
    contain 4 other arguments: a string containing the name of the boat to be
    placed, character followed by a number to indicate the placing position, and
    character either 'V' or 'H' for the boat's placed orientation. For [Quit],
    this would be an input containing the single word "quit", without any other
    miscellaneous characters other than spaces. For [Hit], the input has to
    contain the keyword "hit" followed by 2 arguments indicating its target
    position: a character and a number, all seperated by spaces.

    Examples:

    - "place Submarine A 1 V" is parsed into [Place of "Submarine"]
    - "hit A 1" is parsed into [Hit]
    - "quit " is parsed into [Quit]

    Raises: [Invalid] if the command is invalid— it does not satisfy any of the
    conditions specified for the other commands.*)

val play_game :
  Game.t ->
  string ->
  Game.ship_expose list ->
  Game.ship_expose list ->
  Game.ship_expose list ->
  int ->
  unit
(** [play_game game input boats_list1 boats_list2 original_boats player]
    utilizes the already parsed command from [parse_mode input] to perform
    several actions based on the type of command. For [Place of information] and
    [Hit], this would be updating [player]'s board with the corresponding target
    position of either placing or shooting a boat. It continues running the game
    until it evaluates a [Quit] command in which the game is terminated. *)
