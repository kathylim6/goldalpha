open OUnit2
open Sudoku.Sudokulogic

let four_output =
  "----------\n\
   |. 3 |. 2|\n\
   |. . |4 3|\n\
   ----------\n\
   |2 . |3 .|\n\
   |3 4 |. .|\n\
   ----------\n"

let four_board =
  [|
    [| Empty; Initial 3; Empty; Initial 2 |];
    [| Empty; Empty; Initial 4; Initial 3 |];
    [| Initial 2; Empty; Initial 3; Empty |];
    [| Initial 3; Initial 4; Empty; Empty |];
  |]

let test_string_of_board _ =
  assert_equal four_output (string_of_board four_board)

let valid_cell num = function
  | Empty -> true
  | Initial v -> 1 <= v && v <= num
  | UserInput _ -> false

let rows_are_distinct number board =
  Array.for_all
    (fun row ->
      let seen = Hashtbl.create (number + 1) in
      Array.for_all
        (fun c ->
          match c with
          | Empty -> true
          | Initial v ->
              if Hashtbl.mem seen v then false
              else (
                Hashtbl.add seen v ();
                true)
          | UserInput _ -> false)
        row)
    board

let cols_are_distinct number board =
  let n = Array.length board in
  Array.for_all
    (fun col ->
      let seen = Hashtbl.create (number + 1) in
      Array.for_all
        (fun row ->
          match board.(row).(col) with
          | Empty -> true
          | Initial v ->
              if Hashtbl.mem seen v then false
              else (
                Hashtbl.add seen v ();
                true)
          | UserInput _ -> false)
        (Array.init n Fun.id))
    (Array.init n Fun.id)

let boxes_are_distinct number board =
  let n = Array.length board in
  let box = int_of_float (sqrt (float_of_int n)) in
  let ok = ref true in
  for br = 0 to box - 1 do
    for bc = 0 to box - 1 do
      let seen = Hashtbl.create (number + 1) in
      for r = 0 to box - 1 do
        for c = 0 to box - 1 do
          match board.((br * box) + r).((bc * box) + c) with
          | Empty -> ()
          | Initial v ->
              if Hashtbl.mem seen v then ok := false else Hashtbl.add seen v ()
          | UserInput _ -> ok := false
        done
      done
    done
  done;
  !ok

(* tests for generate_board 4 *)
let test_board_size_4 _ =
  let b = generate_board 4 in
  assert_bool "board must be 4 x 4"
    (Array.length b = 4 && Array.for_all (fun row -> Array.length row = 4) b)

let test_valid_cells_4 _ =
  let b = generate_board 4 in
  assert_bool "all cells must be Initial 1..4 or Empty"
    (Array.for_all (Array.for_all (valid_cell 4)) b)

let test_rows_distinct_4 _ =
  let b = generate_board 4 in
  assert_bool "rows must have distinct non-empty values" (rows_are_distinct 4 b)

let test_cols_distinct_4 _ =
  let b = generate_board 4 in
  assert_bool "columns must have distinct non-empty values"
    (cols_are_distinct 4 b)

let test_boxes_distinct_4 _ =
  let b = generate_board 4 in
  assert_bool "4x4 boxes must have distinct non-empty values"
    (boxes_are_distinct 4 b)

(* tests for generate_board 9 *)
let test_board_size_9 _ =
  let b = generate_board 9 in
  assert_bool "board must be 9 x 9"
    (Array.length b = 9 && Array.for_all (fun row -> Array.length row = 9) b)

let test_valid_cells_9 _ =
  let b = generate_board 9 in
  assert_bool "all cells must be Initial 1..9 or Empty"
    (Array.for_all (Array.for_all (valid_cell 9)) b)

let test_rows_distinct_9 _ =
  let b = generate_board 9 in
  assert_bool "rows must have distinct non-empty values" (rows_are_distinct 9 b)

let test_cols_distinct_9 _ =
  let b = generate_board 9 in
  assert_bool "columns must have distinct non-empty values"
    (cols_are_distinct 9 b)

let test_boxes_distinct_9 _ =
  let b = generate_board 9 in
  assert_bool "9x9 boxes must have distinct non-empty values"
    (boxes_are_distinct 9 b)

(* tests for generate_board 16 *)
let test_board_size_16 _ =
  let b = generate_board 16 in
  assert_bool "board must be 16x16"
    (Array.length b = 16 && Array.for_all (fun row -> Array.length row = 16) b)

let test_valid_cells_16 _ =
  let b = generate_board 16 in
  assert_bool "all cells must be Initial 1..16 or Empty"
    (Array.for_all (Array.for_all (valid_cell 16)) b)

let test_rows_distinct_16 _ =
  let b = generate_board 16 in
  assert_bool "rows must have distinct non-empty values"
    (rows_are_distinct 16 b)

let test_cols_distinct_16 _ =
  let b = generate_board 16 in
  assert_bool "columns must have distinct non-empty values"
    (cols_are_distinct 16 b)

let test_boxes_distinct_16 _ =
  let b = generate_board 16 in
  assert_bool "4x4 boxes must have distinct non-empty values"
    (boxes_are_distinct 16 b)

(* Suite *)
let suite =
  "SudokuTests"
  >::: [
         "string_of_board" >:: test_string_of_board;
         "size 4" >:: test_board_size_4;
         "valid cells 4" >:: test_valid_cells_4;
         "distinct rows 4" >:: test_rows_distinct_4;
         "distinct columns 4" >:: test_cols_distinct_4;
         "distinct boxes 4" >:: test_boxes_distinct_4;
         "size 9" >:: test_board_size_9;
         "valid cells 9" >:: test_valid_cells_9;
         "distinct rows 9" >:: test_rows_distinct_9;
         "distinct columns 9" >:: test_cols_distinct_9;
         "distinct boxes 9" >:: test_boxes_distinct_9;
         "size 16" >:: test_board_size_16;
         "valid cells 16" >:: test_valid_cells_16;
         "distinct rows 16" >:: test_rows_distinct_16;
         "distinct columns 16" >:: test_cols_distinct_16;
         "distinct boxes 16" >:: test_boxes_distinct_16;
       ]

let () = run_test_tt_main suite
