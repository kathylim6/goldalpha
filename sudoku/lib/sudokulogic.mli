(** A cell in a Sudoku board. *)
type cell =
  | Initial of int
    (* A fixed, pre-filled value that is part of the puzzle’s initial state. *)
  | Empty (* An empty cell with no value assigned. *)
  | UserInput of int  (** A value supplied by the user during gameplay. *)

val string_of_board : cell array array -> string
(** [string_of_board board] converts the entire Sudoku board into a printable
    multi-line string.

    Behavior:
    - Determines board size n×n automatically.
    - Computes [root = sqrt n].
    - Uses [string_of_row] to format each row.
    - Inserts horizontal divider lines made of dashes between subgrids and at
      the top/bottom of the board.

    The output is a fully formatted human-readable Sudoku grid. *)

val make_sixteen_board : string -> cell array array
(** [make_sixteen_board] returns a 2D cell array of Sudoku board that is 16x16.
    This function randomly chooses from 10 pre-made 16x16 Sudoku puzzles,
    therefore the puzzle is not randomly generated with the same algorithm as a
    4x4 or 9x9 board.

    The filepath is set within Sudokulogic to the data directory, and is
    included as an argument to help with environment differences in the testing
    file. *)

val choose_random_file_path : unit -> string
(** [choose_random_file_path] randomly chooses one out of 10 of the statically
    typed filepaths in the data directory that contain already curated 16x16
    puzzles*)

val generate_board : int -> cell array array
(** [generate_board n] returns 2D cell array of a Sudoku board, depending on
    [n]:

    - [n = 4] → 4×4 board
    - [n = 9] → 9×9 board
    - any other n will result in an failure

    This function generate puzzles randomly *)
