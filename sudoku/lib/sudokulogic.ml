type cell =
  | Initial of int
  | Empty
  | UserInput of int

(** [let ()] ensures that different random boards are generated each time the
    program runs *)
let () = Random.self_init ()

(* statically typed 16x16 board *)
let sixteen_board =
  [|
    [|
      Initial 14;
      Empty;
      Initial 9;
      Initial 8;
      Empty;
      Empty;
      Empty;
      Initial 4;
      Initial 1;
      Initial 5;
      Empty;
      Empty;
      Initial 2;
      Initial 6;
      Empty;
      Initial 16;
    |];
    [|
      Empty;
      Initial 7;
      Empty;
      Initial 16;
      Initial 3;
      Empty;
      Initial 8;
      Initial 13;
      Initial 12;
      Initial 4;
      Initial 11;
      Empty;
      Initial 15;
      Initial 14;
      Empty;
      Empty;
    |];
    [|
      Initial 13;
      Empty;
      Initial 4;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 15;
      Empty;
      Initial 14;
      Initial 10;
      Initial 5;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Initial 11;
      Empty;
      Initial 10;
      Initial 6;
      Empty;
      Initial 15;
      Initial 1;
      Empty;
      Initial 2;
      Initial 16;
      Empty;
      Initial 9;
      Initial 7;
      Empty;
      Initial 3;
      Empty;
    |];
    [|
      Empty;
      Initial 2;
      Initial 7;
      Initial 1;
      Initial 16;
      Initial 8;
      Empty;
      Initial 12;
      Initial 9;
      Initial 11;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 10;
    |];
    [|
      Initial 16;
      Initial 10;
      Initial 13;
      Empty;
      Initial 9;
      Initial 4;
      Empty;
      Initial 1;
      Initial 5;
      Initial 8;
      Empty;
      Empty;
      Initial 12;
      Empty;
      Initial 6;
      Initial 7;
    |];
    [|
      Initial 12;
      Empty;
      Empty;
      Initial 5;
      Empty;
      Initial 11;
      Empty;
      Empty;
      Empty;
      Initial 2;
      Empty;
      Initial 7;
      Empty;
      Initial 3;
      Empty;
      Initial 9;
    |];
    [|
      Initial 9;
      Empty;
      Empty;
      Empty;
      Initial 5;
      Initial 13;
      Empty;
      Empty;
      Initial 10;
      Initial 12;
      Initial 1;
      Empty;
      Empty;
      Initial 16;
      Initial 2;
      Initial 4;
    |];
    [|
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Empty;
      Initial 12;
      Initial 14;
      Initial 3;
      Initial 13;
      Initial 4;
      Empty;
      Initial 16;
      Initial 8;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 8;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 15;
      Initial 12;
      Empty;
      Empty;
      Initial 9;
      Empty;
      Initial 6;
    |];
    [|
      Initial 1;
      Initial 13;
      Initial 16;
      Initial 2;
      Empty;
      Initial 6;
      Empty;
      Initial 9;
      Initial 11;
      Initial 14;
      Empty;
      Empty;
      Initial 3;
      Empty;
      Initial 4;
      Empty;
    |];
    [|
      Initial 3;
      Initial 11;
      Initial 12;
      Empty;
      Initial 4;
      Empty;
      Empty;
      Empty;
      Initial 6;
      Empty;
      Initial 7;
      Initial 8;
      Initial 13;
      Empty;
      Empty;
      Initial 14;
    |];
    [|
      Initial 10;
      Empty;
      Empty;
      Initial 14;
      Empty;
      Initial 7;
      Initial 5;
      Initial 2;
      Initial 13;
      Initial 6;
      Empty;
      Initial 4;
      Initial 11;
      Initial 1;
      Initial 8;
      Empty;
    |];
    [|
      Empty;
      Empty;
      Initial 11;
      Initial 12;
      Initial 13;
      Empty;
      Empty;
      Initial 3;
      Empty;
      Empty;
      Initial 15;
      Empty;
      Initial 9;
      Initial 7;
      Empty;
      Initial 2;
    |];
    [|
      Empty;
      Initial 9;
      Initial 1;
      Empty;
      Initial 15;
      Empty;
      Initial 6;
      Empty;
      Empty;
      Empty;
      Initial 2;
      Initial 11;
      Initial 4;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 4;
      Initial 3;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 8;
      Empty;
      Empty;
      Empty;
      Initial 12;
      Empty;
      Initial 13;
      Empty;
      Empty;
    |];
  |]

(** [let make_base_board] makes a generic, filled sudoku board *)
let make_base_board size box_size =
  Array.init size (fun row_index ->
      Array.init size (fun col_index ->
          let base =
            (row_index * box_size) + (row_index / box_size) + col_index
          in
          let value = (base mod size) + 1 in
          value))

(* Swaps rows in a sudoku board *)
let swap_rows board row1 row2 =
  let temp = board.(row1) in
  board.(row1) <- board.(row2);
  board.(row2) <- temp

(* Swaps columns in a sudoku board *)
let swap_cols board col1 col2 =
  let size = Array.length board in
  let row_index = ref 0 in
  while !row_index < size do
    let temp = board.(!row_index).(col1) in
    board.(!row_index).(col1) <- board.(!row_index).(col2);
    board.(!row_index).(col2) <- temp;
    row_index := !row_index + 1
  done

(* Shuffles rows that share a box -- boxes are either 2x2 (for a 4x4 board), 3x3
   (for a 9x9 board), or 4x4 (for a 16x16 board) *)
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

(* Shuffles columns that share a box -- boxes are either 2x2 (for a 4x4 board),
   3x3 (for a 9x9 board), or 4x4 (for a 16x16 board) *)
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

(* Makes a sudoku board using our predefined shuffling techniques, converts from
   an integer board to a cell board *)
let make_random_board size box_size =
  let int_board = make_base_board size box_size in
  shuffle_rows_within_group int_board box_size;
  shuffle_cols_within_group int_board box_size;
  Array.init size (fun row_index ->
      Array.init size (fun col_index ->
          Initial int_board.(row_index).(col_index)))

(* Creates a randomly generated, filled 4x4 sudoku board *)
let make_four_board () = make_random_board 4 2

(* Creates a randomly generated, filled 9x9 sudoku board *)
let make_nine_board () = make_random_board 9 3

(* in-line test for board generation *)

let%test "make_four_board: board is 4x4" =
  let b = make_four_board () in
  Array.length b = 4 && Array.for_all (fun row -> Array.length row = 4) b

let%test "make_four_board: all cells are Initial 1..4" =
  let b = make_four_board () in
  Array.for_all
    (Array.for_all (function
      | Initial v -> 1 <= v && v <= 4
      | Empty -> false
      | UserInput _ -> false))
    b

let%test "make_four_board: each row has distinct values" =
  let b = make_four_board () in
  Array.for_all
    (fun row ->
      let vals = Array.to_list row in
      List.length vals = List.length (List.sort_uniq compare vals))
    b

let%test "make_four_board: each column has distinct values" =
  let b = make_four_board () in
  let get_col j = Array.init 4 (fun i -> b.(i).(j)) in
  Array.for_all
    (fun col ->
      let vals = Array.to_list col in
      List.length vals = List.length (List.sort_uniq compare vals))
    (Array.init 4 get_col)

let%test "make_four_board: each 2x2 box has distinct values" =
  let b = make_four_board () in
  let box r c =
    [ b.(r).(c); b.(r).(c + 1); b.(r + 1).(c); b.(r + 1).(c + 1) ]
  in
  List.for_all
    (fun cells ->
      let len = List.length cells in
      len = List.length (List.sort_uniq compare cells))
    [ box 0 0; box 0 2; box 2 0; box 2 2 ]

let%test "make_nine_board: board is 9x9" =
  let b = make_nine_board () in
  Array.length b = 9 && Array.for_all (fun row -> Array.length row = 9) b

let%test "make_nine_board: all cells are Initial 1..9" =
  let b = make_nine_board () in
  Array.for_all
    (Array.for_all (function
      | Initial v -> 1 <= v && v <= 9
      | Empty -> false
      | UserInput _ -> false))
    b

let%test "make_nine_board: rows distinct" =
  let b = make_nine_board () in
  Array.for_all
    (fun row ->
      let vals = Array.to_list row in
      List.length vals = List.length (List.sort_uniq compare vals))
    b

let%test "make_nine_board: cols distinct" =
  let b = make_nine_board () in
  let col j = Array.init 9 (fun i -> b.(i).(j)) in
  Array.for_all
    (fun col ->
      let vals = Array.to_list col in
      List.length vals = List.length (List.sort_uniq compare vals))
    (Array.init 9 col)

let%test "make_nine_board: 3x3 boxes distinct" =
  let b = make_nine_board () in
  let box r c =
    List.concat
      (List.init 3 (fun dr -> List.init 3 (fun dc -> b.(r + dr).(c + dc))))
  in
  List.for_all
    (fun cells ->
      List.length cells = List.length (List.sort_uniq compare cells))
    (List.concat
       (List.init 3 (fun br -> List.init 3 (fun bc -> box (3 * br) (3 * bc)))))

(** let [repeat_string] is a helper function used to print multiple empty spaces
    for proper lining of Sudoku map*)
let repeat_string s n = String.concat "" (List.init n (fun _ -> s))

(** let [pad] is a helper function that takes in a string s and int max, padding
    the appropriate amount of spaces on s for formatting. [max] must be greater
    than 0*)
let pad s max =
  let spaces = max - String.length s in
  s ^ repeat_string " " spaces

(* in-line test cases for [pad] and [repeat_string] *)
let%test _ = repeat_string "a" 3 = "aaa"
let%test _ = repeat_string " " 3 = "   "
let%test _ = repeat_string " " 1 = " "
let%test _ = repeat_string " " 0 = ""
let%test _ = pad "5" 3 = "5  "
let%test _ = pad "3" 1 = "3"
let%test _ = pad "3" 2 = "3 "

(** [string_of_cell c max_len] returns a string representing the cell [c],
    padded with spaces so that its total width equals [max_len].

    - [Empty] is printed as ["."] - [Initial v] prints the integer [v] -
      [UserInput v] prints the integer [v]

    All three are padded with trailing spaces to width [max_len].*)
let string_of_cell c max_len =
  match c with
  | Empty -> pad "." max_len
  | Initial v -> pad (string_of_int v) max_len
  | UserInput v -> pad (string_of_int v) max_len

(* in-line test for [string_of_cell] *)

let%test "string_of_cell works on Empty/Initial/UserInput" =
  string_of_cell Empty 1 = "."
  && string_of_cell (Initial 3) 1 = "3"
  && string_of_cell (UserInput 6) 1 = "6"
  && string_of_cell (UserInput 12) 2 = "12"

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

(* in-line test for [string_of_row] *)
let%test "string_of_row creates rows correctly" =
  string_of_row [| Empty; Initial 1; Empty; Initial 3 |] 2 = "|. 1 |. 3|"
  && string_of_row
       [| Empty; UserInput 2; Initial 3; UserInput 1; Empty; Initial 9 |]
       3
     = "|. 2 3 |1 . 9"

(** [string_of_board] is a function that converts the entire cell array array
    board into a printable string by calling helper functions such as let
    [string_of_row] and let [string_of_cell]*)
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

(** [convert_to_cords] takes in a board represented by an array of array of
    cells and outputs an array of tuples of coordinates. The first two ints
    represent the coordinate position of the cell in the sudoku board.
    [convert_to_cords] is used as a helper in the creation of the puzzle. *)
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

(* Helper: count solutions, stopping at limit *)
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

(** [make_unique] takes in an array of cells that represents a completely filled
    in, valid board. It uses an algorithm to modify this board to make a new
    random puzzle that has a unique solution which will then given to the user*)
let make_unique (board : cell array array) =
  let coords = convert_to_cords board in
  Random.self_init ();
  Array.shuffle ~rand:(fun n -> Random.int n) coords;

  for index = 0 to Array.length coords - 1 do
    let x, y = coords.(index) in
    let number =
      match board.(x).(y) with
      | Initial n -> n
      | _ -> failwith "Unexpected non-initial cell"
    in
    board.(x).(y) <- Empty;

    let num_solutions = count_solutions board 2 in
    if num_solutions <> 1 then board.(x).(y) <- Initial number
  done;
  board

(* Inline tests for valid, find_empty, count_solutions, make_unique *)

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

let empty_n n = Array.make_matrix n n Empty

(* board like full_4x4 but with a single empty at (r,c) let board_with_empty
   base (r, c) = let n = Array.length base in let b = Array.init n (fun i ->
   Array.copy base.(i)) in b.(r).(c) <- Empty; b *)

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

(* Randomly chooses one of the statically-typed, partially filled 16x16 sudoku
   board *)
let make_sixteen_board () = sixteen_board

(** [generate_board] prints a new random Sudoku puzzle to solve, based on either
    dimensions 4, 9, or 16. For a 16x16 Sudoku puzzle, our board will choose
    from 10 Sudoku boards that are statically typed. This is due to runtime
    constraints of our algorithm*)
let generate_board num =
  match num with
  | 4 -> make_unique (make_four_board ())
  | 9 -> make_unique (make_nine_board ())
  | 16 -> make_sixteen_board ()
  | _ -> failwith "Only 4x4, 9x9, and 16x16 boards supported"
