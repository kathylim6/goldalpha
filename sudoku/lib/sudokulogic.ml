type cell =
  | Initial of int
  | Empty
  | UserInput of int

(* Statically typed into four_board *)
let four_board =
  [|
    [| Empty; Initial 3; Empty; Initial 2 |];
    [| Empty; Empty; Initial 4; Initial 3 |];
    [| Initial 2; Empty; Initial 3; Empty |];
    [| Initial 3; Initial 4; Empty; Empty |];
  |]

(* Statically typed into nine_board *)
let nine_board =
  [|
    [|
      Empty;
      Empty;
      Initial 4;
      Empty;
      Initial 1;
      Empty;
      Initial 9;
      Initial 8;
      Empty;
    |];
    [| Empty; Empty; Empty; Empty; Initial 9; Empty; Empty; Empty; Empty |];
    [| Empty; Empty; Empty; Initial 2; Initial 7; Empty; Empty; Empty; Empty |];
    [| Empty; Empty; Empty; Initial 5; Empty; Empty; Empty; Initial 1; Empty |];
    [|
      Empty; Initial 7; Empty; Empty; Empty; Initial 3; Empty; Empty; Initial 4;
    |];
    [| Empty; Initial 9; Empty; Empty; Empty; Empty; Initial 2; Empty; Empty |];
    [|
      Empty;
      Initial 1;
      Empty;
      Initial 6;
      Empty;
      Initial 5;
      Empty;
      Initial 4;
      Empty;
    |];
    [| Initial 7; Empty; Empty; Empty; Initial 3; Empty; Empty; Empty; Empty |];
    [|
      Initial 6; Empty; Empty; Empty; Empty; Empty; Initial 5; Initial 9; Empty;
    |];
  |]

(* Statically typed into sixteen_board *)
let sixteen_board =
  [|
    [|
      Empty;
      Initial 8;
      Empty;
      Empty;
      Empty;
      Initial 2;
      Initial 16;
      Empty;
      Initial 14;
      Empty;
      Initial 11;
      Initial 7;
      Initial 10;
      Empty;
      Initial 4;
      Empty;
    |];
    [|
      Empty;
      Initial 15;
      Empty;
      Empty;
      Empty;
      Initial 8;
      Empty;
      Initial 13;
      Empty;
      Empty;
      Initial 9;
      Empty;
      Empty;
      Initial 16;
      Initial 14;
      Empty;
    |];
    [|
      Initial 14;
      Empty;
      Empty;
      Initial 16;
      Empty;
      Initial 11;
      Initial 12;
      Initial 8;
      Initial 6;
      Initial 2;
      Initial 10;
      Initial 13;
      Initial 5;
      Empty;
      Initial 1;
      Empty;
    |];
    [|
      Empty;
      Empty;
      Initial 11;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 16;
      Initial 7;
      Initial 15;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Initial 6;
      Empty;
      Initial 15;
      Empty;
      Empty;
      Initial 14;
      Initial 7;
      Empty;
      Empty;
      Initial 4;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 13;
      Empty;
    |];
    [|
      Empty;
      Initial 2;
      Empty;
      Empty;
      Initial 1;
      Empty;
      Initial 15;
      Initial 13;
      Initial 3;
      Empty;
      Empty;
      Initial 16;
      Initial 5;
      Initial 11;
      Empty;
      Empty;
    |];
    [|
      Initial 5;
      Empty;
      Initial 3;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 15;
      Initial 8;
      Initial 6;
      Empty;
      Empty;
      Empty;
      Initial 14;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Empty;
      Initial 16;
      Initial 7;
      Initial 3;
      Empty;
      Initial 5;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 2;
      Initial 4;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 6;
      Empty;
      Initial 3;
      Initial 14;
      Initial 9;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 1;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Initial 10;
      Initial 8;
      Initial 4;
      Empty;
      Empty;
      Initial 1;
      Empty;
      Empty;
      Initial 2;
      Empty;
      Empty;
      Empty;
      Initial 12;
      Initial 15;
      Empty;
      Initial 16;
    |];
    [|
      Initial 9;
      Initial 1;
      Initial 5;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Empty;
      Empty;
      Initial 7;
      Empty;
      Empty;
      Initial 3;
      Initial 13;
      Initial 12;
    |];
    [|
      Initial 16;
      Empty;
      Initial 12;
      Empty;
      Empty;
      Empty;
      Initial 6;
      Initial 11;
      Empty;
      Initial 1;
      Empty;
      Initial 13;
      Initial 9;
      Initial 2;
      Initial 8;
      Initial 10;
    |];
    [|
      Empty;
      Initial 3;
      Empty;
      Empty;
      Empty;
      Initial 2;
      Initial 1;
      Initial 4;
      Empty;
      Initial 15;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 11;
      Initial 5;
    |];
    [|
      Initial 11;
      Empty;
      Initial 13;
      Empty;
      Initial 6;
      Empty;
      Empty;
      Initial 5;
      Empty;
      Empty;
      Initial 4;
      Empty;
      Initial 3;
      Empty;
      Initial 8;
      Empty;
    |];
    [|
      Initial 15;
      Initial 4;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 3;
      Empty;
      Empty;
      Initial 2;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 14;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 15;
      Initial 7;
      Initial 5;
      Initial 9;
      Empty;
      Empty;
      Initial 12;
      Initial 13;
      Empty;
      Initial 3;
    |];
  |]

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

(** [generate_board] prints statically typed in boards. Further implementations
    will randomize the boards *)
let generate_board num =
  match num with
  | 4 -> string_of_board four_board
  | 9 -> string_of_board nine_board
  | _ -> string_of_board sixteen_board

(** [convert_to_tuple] takes in a board represented by an array of array of
    cells and outputs an array of tuples of 3 ints. The first two ints represent
    the coordinate position of the cell in the sudoku board, and the third int
    represents the value that is stored in the cell. [convert_to_tuple] is used
    as a helper in the creation of the puzzle. *)
let convert_to_tuple (board : cell array array) : (int * int * int) array =
  let board_dim = Array.length board in
  let result = Array.make (board_dim * board_dim) (0, 0, 0) in
  let k = ref 0 in
  for row = 0 to board_dim - 1 do
    for col = 0 to board_dim - 1 do
      let number =
        match board.(row).(col) with
        | Initial x -> x
        | _ ->
            failwith
              "Should only call [convert_to_tuple] on array of all initial \
               values"
      in
      result.(!k) <- (row, col, number);
      incr k
    done
  done;
  result

(** [convert_to_cell] takes an input board which represents a new random puzzle
    with missing values that has a unique solution. The input board will be
    represented as an array of tuples, where the first two integer values in the
    tuple represents the coordinate values, and the second value represents the
    number in the cell. The input array will not be in order of coordinate
    values. If the number in the cell is -1, then this cell is empty.

    [convert_to_cell] makes this input board an array of array of cells so that
    it can be outputted to the user. *)
let convert_to_cell (board : (int * int * int) array) : cell array array =
  let board_dim = int_of_float (sqrt (float_of_int (Array.length board))) in
  let result = Array.make board_dim (Array.make board_dim Empty) in
  for x = 0 to Array.length board - 1 do
    let tuple_rep = board.(x) in
    let cord_x, cord_y, number = tuple_rep in
    let number_rep =
      match number with
      | -1 -> Empty
      | x -> Initial x
    in
    result.(cord_x).(cord_y) <- number_rep
  done;
  result

let backtracking board = failwith "Unimplemented"

(** [make_unique] takes in an array of cells that represents a completely filled
    in, valid board. It uses an algorithm to modify this board to make a new
    random puzzle that has a unique solution which will then given to the user*)
let make_unique (board : cell array array) =
  Random.self_init ();
  Array.shuffle ~rand:(fun n -> Random.int n) board;

  let tuple_board = convert_to_tuple board in

  for cell = 0 to Array.length board - 1 do
    let x, y, number = tuple_board.(cell) in
    if number != -1 then begin
      tuple_board.(cell) <- (x, y, -1);
      let num_sol = backtracking tuple_board in
      if num_sol > 1 then tuple_board.(cell) <- (x, y, number)
    end
  done;
  let new_board = convert_to_cell tuple_board in
  new_board
