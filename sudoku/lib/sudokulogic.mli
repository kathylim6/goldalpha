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

val generate_board : int -> cell array array
(** [generate_board n] returns 2D cell array of a Sudoku board Sudoku boards,
    depending on [n]:

    - [n = 4] → 4×4 board
    - [n = 9] → 9×9 board
    - [n = 16] → 16×16 board
    - any other n will result in an failure

    This function generate puzzles randomly *)
