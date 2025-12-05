(** AF(cell):
    - Initial n represents a fixed Sudoku cell whose value is n, part of the
      puzzle’s original state.
    - UserInput n represents a user-supplied value n.
    - Empty represents a cell that is currently unfilled.

    RI(cell):
    - For Initial n or UserInput n, the integer n must satisfy: 1 ≤ n ≤
      board_size where board_size is 4, 9, or 16 depending on the puzzle.
    - Empty contains no value.
    - No additional constraints: a single cell does not encode validity
      (validity is enforced at the board level). *)

(**
  A Sudoku board is represented as a 2D array of cells:
      board : cell array array

  AF(board):
    - The board represents an n × n Sudoku grid, where each entry board.(r).(c)
      corresponds to the value in row r and column c of the Sudoku puzzle.
    - Initial cells represent squares fixed by the puzzle design.
    - Empty cells represent squares not yet filled.
    - UserInput cells represent values supplied by the user.

  RI(board):
    - board must be square: 
          let n = Array.length board in
          for all rows r, Array.length board.(r) = n.
    - n must be a valid Sudoku dimension:
          n ∈ {4, 9, 16}.
    - For n = 4, subgrid size is 2×2.
      For n = 9, subgrid size is 3×3.
      For n = 16, subgrid size is 4×4.
    - If a cell is Initial v or UserInput v:
          1 ≤ v ≤ n.
    - Empty cells contain no number.
    - The board is not required to be valid Sudoku (no row/column/box conflicts),
      since validity is checked dynamically via helper functions.
    - For 16×16 boards created via [make_sixteen_board], the file contents must
      match the expected dimensions and numeric constraints.
*)

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
    is 16x16. This function randomly chooses from 3 pre-made 16x16 Sudoku
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
(** [generate_board n level] returns 2D cell array of a Sudoku board, depending
    on [n]. [level] is indicative of the level the user will play:

    - [n = 4] → 4×4 board
    - [n = 9] → 9×9 board
    - any other n will result in an failure

    This function generate puzzles randomly.

    RI(generate_board n level):
    - n must be either 4 or 9; otherwise the function raises an error.
    - The returned board satisfies all representation invariants of a valid
      Sudoku board data structure: • Square n×n shape • Only Initial or Empty
      cells • No repeated values in any row, column, or subgrid among Initial
      cells
    - The number of Empty cells depends on [level], but no structural promise is
      made about their distribution.

    AF(generate_board n level):
    - Returns an abstract Sudoku puzzle of size n whose initial configuration is
      stored as Initial cells, with difficulty loosely controlled by [level]. *)

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

    AF [find_conflicts board r c v]: Represents the set of all board positions
    that would violate Sudoku rules if value v were placed at (r, c). These are
    returned as (row, col) pairs.

    RI(find_conflicts):
    - Must return only positions currently containing v.
    - Must return positions only in: • the same row • the same column • the same
      subgrid
    - Does not modify the board. *)

val is_board_complete : cell array array -> bool
(** [is_board_complete board] returns [true] if all cells are filled (no Empty
    cells).

    AF [is_board_complete board]: Represents the predicate “the board contains
    no Empty cells”. RI:
    - True only if every entry is Initial _ or UserInput _. *)

val is_board_valid : cell array array -> bool
(** [is_board_valid board] returns [true] if the board is completely filled and
    valid (no conflicts in rows, columns, or boxes).

    AF [is_board_valid board]: Represents the predicate “the Sudoku board is
    fully filled and contains no rule violations”.

    RI:
    - Returns true only when: • The board is complete • All rows, columns, and
      subgrids contain no duplicates
    - Does not require the board to be a solvable puzzle, only rule-consistent.
*)
