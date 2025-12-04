type cell =
  | Initial of int
  | Empty
  | UserInput of int

(** This ensures that different random boards are generated each time the
    program runs *)
let () = Random.self_init ()

(** [make_base_board size box_size] makes a generic, filled sudoku board of
    dimensions [size × size] where [box_size] is the dimension of each subgrid.
    Returns a 2D integer array representing a valid, complete sudoku board. *)
let make_base_board size box_size =
  Array.init size (fun row_index ->
      Array.init size (fun col_index ->
          let base =
            (row_index * box_size) + (row_index / box_size) + col_index
          in
          let value = (base mod size) + 1 in
          value))

(** [swap_rows board row1 row2] swaps row [row1] and row [row2] in [board].
    Modifies [board] in place. *)
let swap_rows board row1 row2 =
  let temp = board.(row1) in
  board.(row1) <- board.(row2);
  board.(row2) <- temp

(** [swap_cols board col1 col2] swaps column [col1] and column [col2] in
    [board]. Modifies [board] in place. *)
let swap_cols board col1 col2 =
  let size = Array.length board in
  let row_index = ref 0 in
  while !row_index < size do
    let temp = board.(!row_index).(col1) in
    board.(!row_index).(col1) <- board.(!row_index).(col2);
    board.(!row_index).(col2) <- temp;
    row_index := !row_index + 1
  done

(** [shuffle_rows_within_group board box_size] randomly shuffles rows within
    their respective row groups (horizontal bands) in [board]. A row group
    contains [box_size] rows. This maintains sudoku validity by only swapping
    rows that share the same horizontal band of boxes. Modifies [board] in
    place. *)
let shuffle_rows_within_group board box_size =
  let size = Array.length board in
  let number_of_row_groups = size / box_size in
  let row_group = ref 0 in
  while !row_group < number_of_row_groups do
    let first_row_of_group = !row_group * box_size in
    let position_in_group = ref 0 in
    while !position_in_group < box_size do
      let random_position_in_group =
        !position_in_group + Random.int (box_size - !position_in_group)
      in
      let row1 = first_row_of_group + !position_in_group in
      let row2 = first_row_of_group + random_position_in_group in
      swap_rows board row1 row2;
      position_in_group := !position_in_group + 1
    done;
    row_group := !row_group + 1
  done

(** [shuffle_cols_within_group board box_size] randomly shuffles columns within
    their respective column groups (vertical bands) in [board]. A column group
    contains [box_size] columns. This maintains sudoku validity by only swapping
    columns that share the same vertical band of boxes. Modifies [board] in
    place. *)
let shuffle_cols_within_group board box_size =
  let size = Array.length board in
  let number_of_col_groups = size / box_size in
  let col_group = ref 0 in
  while !col_group < number_of_col_groups do
    let first_col_of_group = !col_group * box_size in
    let position_in_group = ref 0 in
    while !position_in_group < box_size do
      let random_position_in_group =
        !position_in_group + Random.int (box_size - !position_in_group)
      in
      let col1 = first_col_of_group + !position_in_group in
      let col2 = first_col_of_group + random_position_in_group in
      swap_cols board col1 col2;
      position_in_group := !position_in_group + 1
    done;
    col_group := !col_group + 1
  done

(** [make_random_board size box_size] creates a random, completely filled, valid
    sudoku board of dimensions [size × size] with subgrids of size
    [box_size × box_size]. Returns a 2D cell array where all cells are [Initial]
    values. *)
let make_random_board size box_size =
  let int_board = make_base_board size box_size in
  shuffle_rows_within_group int_board box_size;
  shuffle_cols_within_group int_board box_size;
  Array.init size (fun row_index ->
      Array.init size (fun col_index ->
          Initial int_board.(row_index).(col_index)))

(** [make_four_board ()] creates a randomly generated, completely filled 4×4
    sudoku board. Returns a cell array array. *)
let make_four_board () = make_random_board 4 2

(** [make_nine_board ()] creates a randomly generated, completely filled 9×9
    sudoku board. Returns a cell array array. *)
let make_nine_board () = make_random_board 9 3

(** [convert_csv data] converts CSV data (list of string lists) into a cell
    array array. Strings "0" become [Empty] cells, all other numeric strings
    become [Initial] cells with the corresponding integer value. *)
let convert_csv (data : string list list) =
  data
  |> List.map (fun row ->
         row
         |> List.map (function
              | "0" -> Empty
              | s -> Initial (int_of_string s))
         |> Array.of_list)
  |> Array.of_list

[@@@coverage off]

(* in-line test for convert_csv *)
let%test "convert_csv parses a simple 2x3 CSV correctly" =
  let csv = [ [ "1"; "0"; "5" ]; [ "0"; "16"; "3" ] ] in
  let result = convert_csv csv in
  result
  = [| [| Initial 1; Empty; Initial 5 |]; [| Empty; Initial 16; Initial 3 |] |]

let%test "convert_csv handles only zeros" =
  let csv = [ [ "0"; "0" ]; [ "0"; "0" ] ] in
  let r = convert_csv csv in
  r = [| [| Empty; Empty |]; [| Empty; Empty |] |]

[@@@coverage on]

(** [choose_random_file_path ()] randomly chooses one out of 10 of the
    statically typed filepaths in the data directory that contain already
    curated 16x16 puzzles*)
let choose_random_file_path () =
  let rand_int = 1 + Random.int 10 in
  let filepath = "data/16board" ^ string_of_int rand_int ^ ".csv" in
  filepath

(** [make_sixteen_board filepath] returns a cell array array that represents a
    16x16 Sudoku board, based off the CSV input data given through [filepath] *)
let make_sixteen_board filepath =
  let open Csv in
  let data = Csv.load filepath in
  convert_csv data

(** [repeat_string s n] returns a string consisting of [s] repeated [n] times.
    Requires: [n >= 0]. *)
let repeat_string s n = String.concat "" (List.init n (fun _ -> s))

(** [pad s max] returns string [s] padded with trailing spaces so that the total
    length equals [max]. Requires: [max >= String.length s]. *)
let pad s max =
  let spaces = max - String.length s in
  s ^ repeat_string " " spaces

[@@@coverage off]

(* in-line test cases for [pad] and [repeat_string] *)
let%test _ = repeat_string "a" 3 = "aaa"
let%test _ = repeat_string " " 3 = "   "
let%test _ = repeat_string " " 1 = " "
let%test _ = repeat_string " " 0 = ""
let%test _ = pad "5" 3 = "5  "
let%test _ = pad "3" 1 = "3"
let%test _ = pad "3" 2 = "3 "

[@@@coverage on]

(* ANSI color codes *)
let blue = "\027[34m"
let red = "\027[31m"
let orange = "\027[38;5;208m"
let reset = "\027[0m"

(** [string_of_cell c max_len] returns a string representing cell [c], padded
    with trailing spaces to width [max_len]. [Initial] values are colored blue
    using ANSI escape codes, [UserInput] values are default/white color, and
    [Empty] cells display as ".". *)
let string_of_cell c max_len =
  match c with
  | Empty -> pad "." max_len
  | Initial v ->
      let val_str = string_of_int v in
      blue ^ val_str ^ reset
      ^ repeat_string " " (max_len - String.length val_str)
  | UserInput v -> pad (string_of_int v) max_len

(** [string_of_cell_with_conflict c max_len is_conflict] returns a string
    representing cell [c], with special coloring if [is_conflict] is true.
    Conflicting cells are colored red (orange for Initial, regular red for
    UserInput). *)
let string_of_cell_with_conflict c max_len is_conflict =
  match c with
  | Empty -> pad "." max_len
  | Initial v ->
      let val_str = string_of_int v in
      if is_conflict then
        orange ^ val_str ^ reset
        ^ repeat_string " " (max_len - String.length val_str)
      else
        blue ^ val_str ^ reset
        ^ repeat_string " " (max_len - String.length val_str)
  | UserInput v ->
      let val_str = string_of_int v in
      if is_conflict then
        red ^ val_str ^ reset
        ^ repeat_string " " (max_len - String.length val_str)
      else pad val_str max_len

[@@@coverage off]

(* in-line test for [string_of_cell] *)
let%test "string_of_cell works on Empty/Initial/UserInput" =
  string_of_cell Empty 1 = "."
  && string_of_cell (Initial 3) 1 = "\027[34m3\027[0m"
  && string_of_cell (UserInput 6) 1 = "6"
  && string_of_cell (UserInput 12) 2 = "12"

let%test "string_of_cell_with_conflict works on Empty/Initial/UserInput" =
  string_of_cell_with_conflict Empty 1 false = "."
  && string_of_cell_with_conflict (Initial 3) 1 false = "\027[34m3\027[0m"
  && string_of_cell_with_conflict (UserInput 6) 1 false = "6"
  && string_of_cell_with_conflict (UserInput 12) 2 false = "12"

[@@@coverage on]

(** [string_of_row input_row root] converts a full Sudoku row into a 
  formatted string.

   Formatting rules (matching the implementation): - Each cell is printed using
   [string_of_cell]. - Vertical bars ["|"] are inserted at the start of each
   subgrid. A subgrid has width [root]. - A trailing ["|"] appears at the end of
   the row.

   Example for a 4×4 board (root = 2): {| . 1 | . 3 |} *)
let string_of_row input_row root =
  let row_list = Array.to_list input_row in
  let max_len = String.length (string_of_int (root * root)) in
  String.concat " "
    (List.mapi
       (fun i x ->
         if i mod root = 0 then "|" ^ string_of_cell x max_len
         else if i = (root * root) - 1 then string_of_cell x max_len ^ "|"
         else string_of_cell x max_len)
       row_list)

(** [string_of_row_with_conflicts input_row root conflict_positions row_index]
    converts a sudoku row into a formatted string, highlighting cells in
    [conflict_positions] with red coloring. *)
let string_of_row_with_conflicts input_row root conflict_positions row_index =
  let row_list = Array.to_list input_row in
  let max_len = String.length (string_of_int (root * root)) in
  String.concat " "
    (List.mapi
       (fun col_index x ->
         let is_conflict = List.mem (row_index, col_index) conflict_positions in
         let cell_str = string_of_cell_with_conflict x max_len is_conflict in
         if col_index mod root = 0 then "|" ^ cell_str
         else if col_index = (root * root) - 1 then cell_str ^ "|"
         else cell_str)
       row_list)

[@@@coverage off]

(* in-line test for [string_of_row] *)
let%test "string_of_row creates rows correctly" =
  string_of_row [| Empty; Initial 1; Empty; Initial 3 |] 2
  = "|. \027[34m1\027[0m |. \027[34m3\027[0m|"
  && string_of_row
       [| Empty; UserInput 2; Initial 3; UserInput 1; Empty; Initial 9 |]
       3
     = "|. 2 \027[34m3\027[0m |1 . \027[34m9\027[0m"

[@@@coverage on]

(** [string_of_board input_board] is a function that converts the entire cell
    array array board into a printable string by calling helper functions such
    as let [string_of_row] and let [string_of_cell]*)
let string_of_board input_board : string =
  let board_size = float_of_int (Array.length input_board) in
  let root = int_of_float (sqrt board_size) in
  let board_list = Array.to_list input_board in
  String.concat ""
    (List.mapi
       (fun i x ->
         if i mod root = 0 then
           repeat_string "-"
             (int_of_float board_size
              * (2 + abs (1 - String.length (string_of_int (root * root))))
             + root)
           ^ "\n" ^ string_of_row x root ^ "\n"
         else if i = (root * root) - 1 then
           string_of_row x root ^ "\n"
           ^ repeat_string "-"
               (int_of_float board_size
                * (2 + abs (1 - String.length (string_of_int (root * root))))
               + root)
           ^ "\n"
         else string_of_row x root ^ "\n")
       board_list)

(** [string_of_board_with_conflicts input_board conflict_positions] converts the
    entire board into a printable string, highlighting conflicting cells. *)
let string_of_board_with_conflicts input_board conflict_positions : string =
  let board_size = float_of_int (Array.length input_board) in
  let root = int_of_float (sqrt board_size) in
  let board_list = Array.to_list input_board in
  String.concat ""
    (List.mapi
       (fun i x ->
         if i mod root = 0 then
           repeat_string "-"
             (int_of_float board_size
              * (2 + abs (1 - String.length (string_of_int (root * root))))
             + root)
           ^ "\n"
           ^ string_of_row_with_conflicts x root conflict_positions i
           ^ "\n"
         else if i = (root * root) - 1 then
           string_of_row_with_conflicts x root conflict_positions i
           ^ "\n"
           ^ repeat_string "-"
               (int_of_float board_size
                * (2 + abs (1 - String.length (string_of_int (root * root))))
               + root)
           ^ "\n"
         else string_of_row_with_conflicts x root conflict_positions i ^ "\n")
       board_list)

(** [convert_to_cords board] returns an array of all (row, col) coordinate pairs
    in [board], ordered row-major (left-to-right, top-to-bottom). Used as a
    helper for puzzle generation. *)
let convert_to_cords (board : cell array array) : (int * int) array =
  let board_dim = Array.length board in
  let result = Array.make (board_dim * board_dim) (0, 0) in
  let k = ref 0 in
  for row = 0 to board_dim - 1 do
    for col = 0 to board_dim - 1 do
      result.(!k) <- (row, col);
      incr k
    done
  done;
  result

[@@@coverage off]

(* in-line tests for [convert_to_cords] *)
let%test "convert_to_cords: 4x4 board" =
  let board =
    [|
      [| Empty; Initial 4; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
      [| Empty; Empty; Initial 7; Empty |];
      [| Empty; Empty; Empty; Empty |];
    |]
  in
  convert_to_cords board
  = [|
      (0, 0);
      (0, 1);
      (0, 2);
      (0, 3);
      (1, 0);
      (1, 1);
      (1, 2);
      (1, 3);
      (2, 0);
      (2, 1);
      (2, 2);
      (2, 3);
      (3, 0);
      (3, 1);
      (3, 2);
      (3, 3);
    |]

let%test "convert_to_cords 9x9 length" =
  let board = Array.make_matrix 9 9 Empty in
  let coords = convert_to_cords board in
  Array.length coords = 81

(* only spot-check a different coordinates for 9x9 boards and 16x16 boards*)
let%test "convert_to_cords 9x9 ordering" =
  let board = Array.make_matrix 9 9 (Initial 5) in
  let coords = convert_to_cords board in
  coords.(0) = (0, 0)
  && coords.(1) = (0, 1)
  && coords.(8) = (0, 8)
  && coords.(9) = (1, 0)
  && coords.(80) = (8, 8)

let%test "convert_to_cords 16x16 length" =
  let board = Array.make_matrix 16 16 Empty in
  let coords = convert_to_cords board in
  Array.length coords = 256

let%test "convert_to_cords 16x16 ordering spot check" =
  let board = Array.make_matrix 16 16 (Initial 3) in
  let coords = convert_to_cords board in
  coords.(0) = (0, 0)
  && coords.(1) = (0, 1)
  && coords.(15) = (0, 15)
  && coords.(16) = (1, 0)
  && coords.(255) = (15, 15)

[@@@coverage on]

(** [valid board check pos] returns [true] if placing value [check] at position
    [pos] (row, col) would be valid according to sudoku rules (no duplicates in
    row, column, or subgrid). Returns [false] if the placement violates any
    rule. Only checks [Initial] cells; raises [Failure] if it encounters
    [UserInput]. *)
let valid (board : cell array array) (check : int) (pos : int * int) : bool =
  let row = fst pos in
  let col = snd pos in
  let dim = Array.length board in

  (* Helper: return true if a cell contains the number check *)
  let cell_matches n =
    match n with
    | Initial x -> x = check
    | Empty -> false
    | UserInput _ -> failwith "Unexpected user input cell"
  in
  try
    (* Row check *)
    for c = 0 to dim - 1 do
      if c <> col && cell_matches board.(row).(c) then raise Exit
    done;

    (* Column check *)
    for r = 0 to dim - 1 do
      if r <> row && cell_matches board.(r).(col) then raise Exit
    done;

    (* Subgrid size: 4 x 4 -> 2, 9×9 → 3, 16×16 → 4 *)
    let sub = int_of_float (sqrt (float_of_int dim)) in

    let box_r = row / sub * sub in
    let box_c = col / sub * sub in

    (* Subgrid check *)
    for r = box_r to box_r + sub - 1 do
      for c = box_c to box_c + sub - 1 do
        if (not (r = row && c = col)) && cell_matches board.(r).(c) then
          raise Exit
      done
    done;

    true
  with Exit -> false

(** [find_empty board] returns [Some (row, col)] for the first empty cell found
    in row-major order, or [None] if the board is completely filled. Raises
    [Failure] if it encounters a [UserInput] cell. *)
let find_empty (board : cell array array) : (int * int) option =
  let dim = Array.length board in
  let rec search_row r =
    if r = dim then None
    else
      let rec search_col c =
        if c = dim then search_row (r + 1)
        else
          match board.(r).(c) with
          | Empty -> Some (r, c)
          | Initial _ -> search_col (c + 1)
          | UserInput _ -> failwith "Unexpected user input cell"
      in
      search_col 0
  in
  search_row 0

(** [count_solutions board limit] counts the number of valid solutions for
    [board], stopping once [limit] solutions are found. Uses backtracking to
    explore possible completions. Returns the count (up to [limit]). *)
let count_solutions board limit =
  let dim = Array.length board in
  let rec backtrack count =
    if count >= limit then count
    else
      match find_empty board with
      | None -> count + 1
      | Some (row, col) ->
          let total = ref count in
          for check = 1 to dim do
            if !total < limit && valid board check (row, col) then begin
              board.(row).(col) <- Initial check;
              total := backtrack !total;
              board.(row).(col) <- Empty
            end
          done;
          !total
  in
  backtrack 0

(** [make_unique board] modifies [board] to create a sudoku puzzle with a unique
    solution by removing cells one at a time. Starts with a complete, valid
    board and removes values while ensuring exactly one solution remains.
    Returns the modified board. *)
let make_unique board =
  let coords = convert_to_cords board in
  Random.self_init ();
  Array.shuffle ~rand:(fun n -> Random.int n) coords;

  let rec remove index =
    if index = Array.length coords then true
    else
      let r, c = coords.(index) in
      match board.(r).(c) with
      | Empty -> remove (index + 1)
      | Initial n ->
          board.(r).(c) <- Empty;

          if count_solutions board 2 = 1 then
            (* keep removed, continue *)
            remove (index + 1)
          else begin
            (* revert, try skipping removal *)
            board.(r).(c) <- Initial n;
            remove (index + 1)
          end
      | UserInput _ -> failwith "Unexpected user input cell"
  in

  ignore (remove 0);
  board

let full_4x4 : cell array array =
  [|
    [| Initial 1; Initial 2; Initial 3; Initial 4 |];
    [| Initial 3; Initial 4; Initial 1; Initial 2 |];
    [| Initial 2; Initial 1; Initial 4; Initial 3 |];
    [| Initial 4; Initial 3; Initial 2; Initial 1 |];
  |]

let puzzle_4x4 : cell array array =
  [|
    [| Empty; Empty; Empty; Initial 4 |];
    [| Empty; Empty; Empty; Empty |];
    [| Initial 2; Empty; Empty; Initial 3 |];
    [| Initial 4; Empty; Initial 1; Initial 2 |];
  |]

(** [empty_n n] creates an [n × n] board filled entirely with [Empty] cells. *)
let empty_n n = Array.make_matrix n n Empty

[@@@coverage off]

(* in-line tests for checking [valid] *)

let%test "valid: permit placing value when not present in row/col/box (4x4)" =
  let b = puzzle_4x4 in
  valid b 4 (2, 2)

let%test "valid: reject row conflict (4x4)" =
  (* row 0 of full_4x4 already contains a 1, trying to place 1 at (0,1) should
     be false *)
  let b = puzzle_4x4 in
  not (valid b 3 (2, 2))

let%test "valid: reject column conflict (9x9)" =
  (* Build a small controlled 9x9 column conflict: put Initial 5 at (0,2), then
     attempting to place 5 at (3,2) should be rejected *)
  let b = empty_n 9 in
  b.(0).(2) <- Initial 5;
  b.(1).(2) <- Initial 1;
  b.(2).(2) <- Initial 2;
  (* position (3,2) empty; placing 5 should be invalid because column already
     has 5 *)
  not (valid b 5 (3, 2))

let%test "valid: reject subgrid (box) conflict (9x9)" =
  (* place a 7 in the top-left 3x3 box then test a conflict inside same box *)
  let b = empty_n 9 in
  b.(0).(0) <- Initial 7;
  (* trying to place 7 at (1,1) should fail due to box conflict *)
  not (valid b 7 (1, 1))

let%test "find_empty: finds first empty in row-major order (4x4)" =
  let b = Array.init 4 (fun i -> Array.make 4 (Initial 1)) in
  b.(0).(0) <- Empty;
  (* first cell empty => should return (0,0) *)
  match find_empty b with
  | Some (0, 0) -> true
  | _ -> false

let%test "find_empty: returns None for a full board (4x4)" =
  match find_empty full_4x4 with
  | None -> true
  | Some _ -> false

let%test "find_empty: finds first empty in 9x9 (row-major)" =
  let b = empty_n 9 in
  b.(0).(5) <- Initial 1;
  (* set some initials but keep (0,0) empty *)
  match find_empty b with
  | Some (0, 0) -> true
  | _ -> false

let%test "count_solutions: complete board -> exactly 1 solution (4x4)" =
  count_solutions (Array.init 4 (fun i -> Array.copy full_4x4.(i))) 2 = 1

let%test "count_solutions: empty 4x4 hits limit quickly (limit = 2)" =
  (* completely empty 4x4 has many solutions; with limit 2 algorithm should
     return 2 *)
  count_solutions (empty_n 4) 2 = 2

let%test "count_solutions: almost empty 9x9 respects limit (limit = 2)" =
  (* large empty region; we only care that it reaches the limit *)
  count_solutions (empty_n 9) 2 = 2

let%test "make_unique: produced puzzle has unique solution (4x4)" =
  (* Start from a valid full 4x4, run make_unique, then assert the produced
     board has exactly 1 solution. *)
  let b = Array.init 4 (fun i -> Array.copy full_4x4.(i)) in
  ignore (make_unique b);
  count_solutions b 2 = 1

let%test "make_unique: preserves initial values only as needed (4x4)" =
  (* make_unique should only leave Initials where necessary to preserve
     uniqueness. We assert that after make_unique, every non-empty cell is
     Initial and numbers are in range. *)
  let b = Array.init 4 (fun i -> Array.copy full_4x4.(i)) in
  ignore (make_unique b);
  Array.for_all
    (Array.for_all (function
      | Initial v -> 1 <= v && v <= 4
      | Empty -> true
      | _ -> false))
    b

[@@@coverage on]

(** [generate_board num] prints a new random Sudoku puzzle to solve, based on
    either dimensions 4 or 9. *)
let generate_board num =
  match num with
  | 4 -> make_unique (make_four_board ())
  | 9 -> make_unique (make_nine_board ())
  | _ -> failwith "Only 4x4 and 9x9 boards supported for random generation"

(** [check_valid_row user_input board row] checks that there are no overlapping
    values between the user input and the row the user place the number in *)
let check_valid_row (user_input : int) (board : cell array array) (row : int) :
    bool =
  let row_to_check = board.(row) in
  let converted_input_initial = Initial user_input in
  let converted_input_exisintg = UserInput user_input in
  let res =
    Array.mem converted_input_initial row_to_check
    || Array.mem converted_input_exisintg row_to_check
  in
  res

let board =
  [|
    [| Initial 1; UserInput 2; Empty |];
    [| Empty; Initial 4; UserInput 4 |];
    [| Empty; Empty; Empty |];
  |]

[@@@coverage off]

(* in-line test to make sure that generated board actually has one unique
   solution *)
let%test "generated 4x4 puzzle has one unique solution" =
  let b = generate_board 4 in
  count_solutions b 100 = 1

let%test "generated 9x9 puzzle has one unique solution" =
  let b = generate_board 9 in
  count_solutions b 100 = 1

(* in-line tests for check_valid_row *)

let%test "row0 has conflict with an initial cell generated by game" =
  check_valid_row 1 board 0 = true

let%test "row0 has conflict with a user_input cell" =
  check_valid_row 2 board 0 = true

let%test "row0_no_conflict" = check_valid_row 3 board 0 = false
let%test "row1_conflict_dual" = check_valid_row 4 board 1 = true
let%test "row1_no_conflict" = check_valid_row 9 board 1 = false
let%test "empty_row" = check_valid_row 5 board 2 = false

[@@@coverage on]

(** [convert_column_to_arry game_board column_to_convert] extracts column
    [column_to_convert] from [game_board] and returns it as a cell array. *)
let convert_column_to_arry (game_board : cell array array)
    (column_to_convert : int) : cell array =
  let height_of_board = Array.length game_board in
  let new_arr = Array.make height_of_board Empty in
  for i = 0 to height_of_board - 1 do
    new_arr.(i) <- game_board.(i).(column_to_convert)
  done;
  new_arr

(** [check_valid_col user_input board column] checks that there are no
    overlapping values between the user input and the column the user places the
    number in *)
let check_valid_col (user_input : int) (board : cell array array) (column : int)
    : bool =
  let arr_to_check = convert_column_to_arry board column in
  let converted_input_initial = Initial user_input in
  let converted_input_existing = UserInput user_input in
  let res =
    Array.mem converted_input_initial arr_to_check
    || Array.mem converted_input_existing arr_to_check
  in
  res

(** [generate_box_array board row col] extracts all cells from the subgrid/box
    containing position ([row], [col]) and returns them as a cell array. *)
let generate_box_array (board : cell array array) (row : int) (col : int) :
    cell array =
  let n = Array.length board in
  let k = int_of_float (sqrt (float_of_int n)) in

  let new_arr = Array.make (k * k) Empty in

  let row_base = row / k * k in
  let col_base = col / k * k in

  let idx = ref 0 in
  for r = row_base to row_base + k - 1 do
    for c = col_base to col_base + k - 1 do
      new_arr.(!idx) <- board.(r).(c);
      incr idx
    done
  done;

  new_arr

(** [check_valid_box user_input board row col] checks that there are no
    overlapping values between the user input and the box the user places the
    number in *)
let check_valid_box (user_input : int) (board : cell array array) (row : int)
    (col : int) : bool =
  let array_to_check = generate_box_array board row col in
  let converted_input_existing = Initial user_input in
  let converted_input_user = UserInput user_input in
  let res =
    Array.mem converted_input_existing array_to_check
    || Array.mem converted_input_user array_to_check
  in
  res

(** [check_invalid_input input row col board] returns a boolean which is
    representative of whether the user's input follows the constraints of the
    Sudoku game or not *)
let check_invalid_input (input : int) (row : int) (col : int)
    (board : cell array array) : bool =
  let violates_row = check_valid_row input board row in
  let violates_col = check_valid_col input board col in
  let violates_box = check_valid_box input board row col in
  violates_row || violates_col || violates_box

(** [find_conflicts board row col value] returns a list of (row, col) positions
    that conflict with placing [value] at position ([row], [col]). Includes
    conflicts in the same row, column, and box. *)
let find_conflicts board row col value =
  let n = Array.length board in
  let box_size = int_of_float (sqrt (float_of_int n)) in
  let conflicts = ref [] in

  (* Helper to check if cell matches the value *)
  let cell_matches r c =
    match board.(r).(c) with
    | Initial v -> v = value
    | UserInput v -> v = value
    | Empty -> false
  in

  (* Check row conflicts *)
  for c = 0 to n - 1 do
    if c <> col && cell_matches row c then conflicts := (row, c) :: !conflicts
  done;

  (* Check column conflicts *)
  for r = 0 to n - 1 do
    if r <> row && cell_matches r col then conflicts := (r, col) :: !conflicts
  done;

  (* Check box conflicts *)
  let box_row = row / box_size * box_size in
  let box_col = col / box_size * box_size in
  for r = box_row to box_row + box_size - 1 do
    for c = box_col to box_col + box_size - 1 do
      if (r <> row || c <> col) && cell_matches r c then
        if not (List.mem (r, c) !conflicts) then
          conflicts := (r, c) :: !conflicts
    done
  done;

  !conflicts

(** [is_board_complete board] returns [true] if all cells on the board are
    filled (no Empty cells remain), [false] otherwise. *)
let is_board_complete board =
  Array.for_all
    (Array.for_all (function
      | Empty -> false
      | Initial _ | UserInput _ -> true))
    board

(** [is_board_valid board] returns [true] if the board is completely filled and
    has no conflicts (all rows, columns, and boxes contain distinct values). *)
let is_board_valid board =
  let n = Array.length board in
  let box_size = int_of_float (sqrt (float_of_int n)) in

  (* Check all rows *)
  let rows_valid =
    Array.for_all
      (fun row ->
        let values =
          Array.fold_left
            (fun acc cell ->
              match cell with
              | Initial v | UserInput v -> v :: acc
              | Empty -> acc)
            [] row
        in
        List.length values = List.length (List.sort_uniq compare values))
      board
  in

  (* Check all columns *)
  let cols_valid =
    let rec check_col col =
      if col >= n then true
      else
        let values =
          Array.fold_left
            (fun acc row ->
              match row.(col) with
              | Initial v | UserInput v -> v :: acc
              | Empty -> acc)
            [] board
        in
        List.length values = List.length (List.sort_uniq compare values)
        && check_col (col + 1)
    in
    check_col 0
  in

  (* Check all boxes *)
  let boxes_valid =
    let rec check_box box_row box_col =
      if box_row >= n then true
      else if box_col >= n then check_box (box_row + box_size) 0
      else
        let values = ref [] in
        for r = box_row to box_row + box_size - 1 do
          for c = box_col to box_col + box_size - 1 do
            match board.(r).(c) with
            | Initial v | UserInput v -> values := v :: !values
            | Empty -> ()
          done
        done;
        let sorted = List.sort_uniq compare !values in
        List.length !values = List.length sorted
        && check_box box_row (box_col + box_size)
    in
    check_box 0 0
  in

  rows_valid && cols_valid && boxes_valid
