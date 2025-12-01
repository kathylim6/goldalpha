open Sudokutypes

(* Ensures that different random boards are generated each time the program
   runs *)
let () = Random.self_init ()

(* Makes a generic, filled sudoku board *)
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

(* Randomly chooses one of the statically-typed, partially filled 16x16 sudoku
   board *)
let make_sixteen_board () =
  let all = Sixteen_boards.boards in
  let len = List.length all in
  let idx = Random.int len in
  List.nth all idx

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

(** let [string_of_cell] converts the cells of cell type into strings *)
let string_of_cell c max_len =
  match c with
  | Empty -> pad "." max_len
  | Initial v -> pad (string_of_int v) max_len
  | UserInput v -> pad (string_of_int v) max_len

(** let [string_of_row] converts a row of the board from cell array format into
    a single-line string *)
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

(** let [string_of_board] is a function that converts the entire cell array
    array board into a printable string by calling helper functions such as let
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

(** [generate_board] prints a new random Sudoku puzzle to solve, based on either
    dimensions 4, 9, or 16. For a 16x16 Sudoku puzzle, our board will choose
    from 10 Sudoku boards that are statically typed. This is due to runtime
    constraints of our algorithm*)
let generate_board num =
  match num with
  | 4 -> make_unique (make_four_board ())
  | 9 -> make_unique (make_nine_board ())
  | 16 -> Array.map Array.copy (make_sixteen_board ())
  | _ -> failwith "Only 4x4, 9x9, and 16x16 boards supported"
