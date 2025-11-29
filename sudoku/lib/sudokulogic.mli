(** A cell in a Sudoku board. *)
type cell =
  | Initial of int
      (** A fixed, pre-filled value that is part of the puzzle’s initial state.
      *)
  | Empty  (** An empty cell with no value assigned. *)
  | UserInput of int  (** A value supplied by the user during gameplay. *)

val four_board : cell array array
(** A fixed 4×4 Sudoku board, statically defined in the implementation. *)

val nine_board : cell array array
(** A fixed 9×9 Sudoku board, statically defined in the implementation. *)

val sixteen_board : cell array array
(** A fixed 16×16 Sudoku board, statically defined in the implementation. *)

val string_of_cell : cell -> int -> string
(** [string_of_cell c max_len] returns a string representing the cell [c],
    padded with spaces so that its total width equals [max_len].

    - [Empty] is printed as ["."]
    - [Initial v] prints the integer [v]
    - [UserInput v] prints the integer [v]

    All three are padded with trailing spaces to width [max_len].*)

val string_of_row : cell array -> int -> string
(** [string_of_row row root] converts a full Sudoku row into a formatted string.

    Formatting rules (matching the implementation):
    - Each cell is printed using [string_of_cell].
    - Vertical bars ["|"] are inserted at the start of each subgrid.
      A subgrid has width [root].
    - A trailing ["|"] appears at the end of the row.

    Example for a 4×4 board (root = 2):
    {| . 1 | . 3 |}*)

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

val generate_board : int -> string
(** [generate_board n] returns the formatted string of one of the predefined
    Sudoku boards, depending on [n]:

    - [n = 4] → 4×4 board
    - [n = 9] → 9×9 board
    - any other value → 16×16 board

    This function does not generate puzzles randomly; it simply selects from the
    statically defined boards. *)

val make_unique : cell array -> cell array
