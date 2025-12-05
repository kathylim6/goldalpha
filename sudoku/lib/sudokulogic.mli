(** A cell in a Sudoku board. *)
type cell =
  | Initial of int
    (* A fixed, pre-filled value that is part of the puzzle’s initial state. *)
  | Empty (* An empty cell with no value assigned. *)
  | UserInput of int  (** A value supplied by the user during gameplay. *)

val string_of_board : cell array array -> string
(** [string_of_board board] converts the entire Sudoku board into a printable
    multi-line string.

    It uses ANSI Terminal coloring to differentiate between initial cell values,
    user input cell values, and any violations. Below are the coloring codes for
    our puzzle
    - Blue: Initial values given by puzzle
    - Black: User input
    - Orange: If there is a conflict and the cell is an initial value
    - Red: If there is a conflict and the cell is a user input

    The output is a fully formatted human-readable Sudoku grid. *)

val make_sixteen_board : string -> cell array array
(** [make_sixteen_board filepath] returns a 2D cell array of Sudoku board that
    is 16x16. This function randomly chooses from 10 pre-made 16x16 Sudoku
    puzzles by reading the appropriate CSV file from the filepath, therefore the
    puzzle is not randomly generated with the same algorithm as a 4x4 or 9x9
    board.

    The filepath is set within Sudokulogic to the data directory, and is
    included as an argument to help with environment differences in the testing
    file. *)

val choose_random_file_path : unit -> string
(** [choose_random_file_path] randomly chooses one out of 10 of the statically
    typed filepaths in the data directory that contain already curated 16x16
    puzzles*)

val generate_board : int -> int -> cell array array
(** [generate_board n] returns 2D cell array of a Sudoku board, depending on
    [n]:

    - [n = 4] → 4×4 board
    - [n = 9] → 9×9 board
    - any other n will result in an failure

    This function generate puzzles randomly *)

val check_invalid_input : int -> int -> int -> cell array array -> bool
(** [check_invalid_input] returns a boolean which dictates whether or not the
    user's input in the Sudoku board meets the constraints of the game rules *)

val string_of_board_with_conflicts :
  cell array array -> (int * int) list -> string
(** [string_of_board_with_conflicts board conflicts] converts the board to a
    formatted string with cells at positions in [conflicts] highlighted in red.

    - Conflicting [Initial] cells are shown in bright red
    - Conflicting [UserInput] cells are shown in regular red
    - Non-conflicting cells use normal coloring (blue for Initial, white for
      UserInput)

    This is used to display invalid move attempts without modifying the actual
    board. *)

val find_conflicts : cell array array -> int -> int -> int -> (int * int) list
(** [find_conflicts board row col value] returns a list of (row, col) positions
    that would conflict with placing [value] at position ([row], [col]).

    Checks for conflicts in:
    - The same row
    - The same column
    - The same subgrid/box

    Returns positions of all cells containing [value] that violate Sudoku rules.
*)

val is_board_complete : cell array array -> bool
(** [is_board_complete board] returns [true] if all cells are filled (no Empty
    cells). *)

val is_board_valid : cell array array -> bool
(** [is_board_valid board] returns [true] if the board is completely filled and
    valid (no conflicts in rows, columns, or boxes). *)
