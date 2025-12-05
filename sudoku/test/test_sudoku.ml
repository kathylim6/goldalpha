open OUnit2
open Sudoku.Sudokulogic

let four_output =
  "----------\n\
   |. \027[34m3\027[0m |. \027[34m2\027[0m|\n\
   |. . |\027[34m4\027[0m \027[34m3\027[0m|\n\
   ----------\n\
   |\027[34m2\027[0m . |\027[34m3\027[0m .|\n\
   |\027[34m3\027[0m \027[34m4\027[0m |. .|\n\
   ----------\n"

let four_board =
  [|
    [| Empty; Initial 3; Empty; Initial 2 |];
    [| Empty; Empty; Initial 4; Initial 3 |];
    [| Initial 2; Empty; Initial 3; Empty |];
    [| Initial 3; Initial 4; Empty; Empty |];
  |]

let test_string_of_board _ =
  assert_equal ~printer:(fun s -> s) four_output (string_of_board four_board)

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
  let b = generate_board 4 3 in
  assert_bool "board must be 4 x 4"
    (Array.length b = 4 && Array.for_all (fun row -> Array.length row = 4) b)

let test_valid_cells_4 _ =
  let b = generate_board 4 3 in
  assert_bool "all cells must be Initial 1..4 or Empty"
    (Array.for_all (Array.for_all (valid_cell 4)) b)

let test_rows_distinct_4 _ =
  let b = generate_board 4 3 in
  assert_bool "rows must have distinct non-empty values" (rows_are_distinct 4 b)

let test_cols_distinct_4 _ =
  let b = generate_board 4 3 in
  assert_bool "columns must have distinct non-empty values"
    (cols_are_distinct 4 b)

let test_boxes_distinct_4 _ =
  let b = generate_board 4 3 in
  assert_bool "4x4 boxes must have distinct non-empty values"
    (boxes_are_distinct 4 b)

(* tests for generate_board 9 *)
let test_board_size_9 _ =
  let b = generate_board 9 3 in
  assert_bool "board must be 9 x 9"
    (Array.length b = 9 && Array.for_all (fun row -> Array.length row = 9) b)

let test_valid_cells_9 _ =
  let b = generate_board 9 3 in
  assert_bool "all cells must be Initial 1..9 or Empty"
    (Array.for_all (Array.for_all (valid_cell 9)) b)

let test_rows_distinct_9 _ =
  let b = generate_board 9 3 in
  assert_bool "rows must have distinct non-empty values" (rows_are_distinct 9 b)

let test_cols_distinct_9 _ =
  let b = generate_board 9 3 in
  assert_bool "columns must have distinct non-empty values"
    (cols_are_distinct 9 b)

let test_boxes_distinct_9 _ =
  let b = generate_board 9 3 in
  assert_bool "9x9 boxes must have distinct non-empty values"
    (boxes_are_distinct 9 b)

(* tests for [choose_random_file_path] *)
let extract_number path =
  let len_prefix = String.length "data/16board" in
  let len_suffix = String.length ".csv" in
  let middle =
    String.sub path len_prefix (String.length path - len_prefix - len_suffix)
  in
  int_of_string middle

let test_starts_with _ =
  let p = choose_random_file_path () in
  assert_bool "Path should start with data/16board"
    (String.starts_with ~prefix:"data/16board" p)

let test_ends_with _ =
  let p = choose_random_file_path () in
  assert_bool "Path should end with .csv" (String.ends_with ~suffix:".csv" p)

let test_number_in_range _ =
  let p = choose_random_file_path () in
  let n = extract_number p in
  assert_bool "File number should be 1..10" (1 <= n && n <= 10)

let test_randomness_produces_variety _ =
  let samples = List.init 30 (fun _ -> choose_random_file_path ()) in
  let uniq = List.sort_uniq compare samples in
  assert_bool "Randomness should produce more than one distinct output"
    (List.length uniq > 1)

(* tests for make_sixteen_board 16; we iterate through all 3 filepaths to ensure
   they are correct. *)
let all_paths =
  List.init 3 (fun i -> Printf.sprintf "../data/16board%d.csv" (i + 1))

let is_16x16 b =
  Array.length b = 16 && Array.for_all (fun row -> Array.length row = 16) b

let all_cells_valid b = Array.for_all (Array.for_all (valid_cell 16)) b
let rows_ok b = rows_are_distinct 16 b
let cols_ok b = cols_are_distinct 16 b
let boxes_ok b = boxes_are_distinct 16 b

let sample_board =
  [|
    [|
      Initial 5; Initial 3; Empty; Empty; Initial 7; Empty; Empty; Empty; Empty;
    |];
    [|
      Initial 6;
      Empty;
      Initial 1;
      Initial 9;
      Initial 5;
      Empty;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 9;
      UserInput 8;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 6;
      Empty;
    |];
    [|
      Initial 8;
      Empty;
      Empty;
      Empty;
      Initial 6;
      Empty;
      Empty;
      Empty;
      UserInput 3;
    |];
    [|
      Initial 4;
      Empty;
      Empty;
      Initial 8;
      Empty;
      Initial 3;
      Empty;
      Empty;
      Initial 1;
    |];
    [|
      Initial 7; Empty; Empty; Empty; Initial 2; Empty; Empty; Empty; Initial 6;
    |];
    [|
      Empty; Initial 6; Empty; Empty; Empty; Empty; Initial 2; Initial 8; Empty;
    |];
    [|
      Empty;
      Empty;
      Empty;
      Initial 4;
      Initial 1;
      Initial 9;
      Empty;
      Empty;
      Initial 5;
    |];
    [|
      Empty; Empty; Empty; Empty; Initial 8; Empty; Empty; Initial 7; Initial 9;
    |];
  |]

let test_row_conflict _ =
  let result = check_invalid_input 3 0 2 sample_board in
  assert_bool "Placing 3 in row 0 should be invalid" result

let test_column_conflict _ =
  let result = check_invalid_input 6 5 0 sample_board in
  assert_bool "Placing 6 in column 0 should be invalid" result

let test_box_conflict _ =
  let result = check_invalid_input 9 2 0 sample_board in
  assert_bool "Placing 9 in top-left subgrid should be invalid" result

let test_valid_move _ =
  let result = check_invalid_input 2 0 2 sample_board in
  assert_bool "Placing 2 at (0,2) should be valid" (not result)

let make_tests_for_file path =
  let name_base = Filename.basename path in
  [
    ( name_base ^ " size_16x16" >:: fun _ ->
      let b = make_sixteen_board path in
      assert_bool "board must be 16x16" (is_16x16 b) );
    ( name_base ^ " valid_cells" >:: fun _ ->
      let b = make_sixteen_board path in
      assert_bool "all cells must be Initial 1..16 or Empty" (all_cells_valid b)
    );
    ( name_base ^ " rows_distinct" >:: fun _ ->
      let b = make_sixteen_board path in
      assert_bool "rows must have distinct non-empty values" (rows_ok b) );
    ( name_base ^ " cols_distinct" >:: fun _ ->
      let b = make_sixteen_board path in
      assert_bool "columns must have distinct non-empty values" (cols_ok b) );
    ( name_base ^ " boxes_distinct" >:: fun _ ->
      let b = make_sixteen_board path in
      assert_bool "4x4 boxes must have distinct non-empty values" (boxes_ok b)
    );
  ]

let test_row_conflicts _ =
  (* Row 0 has two 5s *)
  let board =
    [|
      [| UserInput 5; Initial 5; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
    |]
  in
  let conflicts = find_conflicts board 0 0 5 in
  assert_equal [ (0, 1) ] conflicts

let test_col_conflicts _ =
  (* Column 0 has two 7s *)
  let board =
    [|
      [| UserInput 7; Empty; Empty; Empty |];
      [| Initial 7; Empty; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
    |]
  in
  let conflicts = find_conflicts board 0 0 7 in
  assert_equal [ (1, 0) ] conflicts

let test_box_conflicts _ =
  (* 2x2 box: row0: [3 .] row1: [. 3] placing 3 at (0,0) should detect conflict
     at (1,1) *)
  let board =
    [|
      [| UserInput 3; Empty; Empty; Empty |];
      [| Empty; Initial 3; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
    |]
  in
  let conflicts = find_conflicts board 0 0 3 in
  assert_equal [ (1, 1) ] conflicts

let test_no_conflicts _ =
  (* This board has no conflicts *)
  let board =
    [|
      [| UserInput 2; Initial 1; Empty; Empty |];
      [| Initial 4; Initial 3; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
      [| Empty; Empty; Empty; Empty |];
    |]
  in

  let conflicts = find_conflicts board 0 0 2 in

  (* Should contain exactly these 3 unique conflicts *)
  let expected = [] in
  assert_equal expected conflicts

let test_string_of_board_with_conflicts _ =
  (* ANSI colors used by our printer *)
  let red = "\027[31m" in
  let blue = "\027[34m" in
  let orange = "\027[38;5;208m" in
  let reset = "\027[0m" in

  (* Small 2×2 board so we can predict output exactly *)
  let board = [| [| Initial 1; UserInput 1 |]; [| Empty; Initial 3 |] |] in

  (* mark the conflicts *)
  let conflicts = [ (0, 0); (0, 1) ] in

  (* Call function under test *)
  let actual = string_of_board_with_conflicts board conflicts in

  let horizontal = "-----\n" in

  let row0 = "|" ^ (orange ^ "1" ^ reset) ^ " |" ^ (red ^ "1" ^ reset) ^ "\n" in

  let row1 = "|" ^ "." ^ " |" ^ (blue ^ "3" ^ reset) ^ "\n" in

  let expected = horizontal ^ row0 ^ horizontal ^ row1 in

  assert_equal ~printer:(fun s -> s) expected actual

(* this test case differs from the one above because instead of statically
   typing the conflicts, the test cases calls on [find_conflict] to build
   complete list of conflicts for board*)
let test_conflict_workflow _ =
  let red = "\027[31m" in
  let blue = "\027[34m" in
  let orange = "\027[38;5;208m" in
  let reset = "\027[0m" in
  let horizontal = "----------\n" in

  let board =
    [|
      [| UserInput 1; Empty; Initial 1; Empty |];
      [| Empty; Initial 3; Empty; Empty |];
      [| Empty; Initial 1; Empty; Initial 4 |];
      [| Initial 2; Empty; Empty; Empty |];
    |]
  in

  (* instead of statically typing the conflicts, call find_conflicts to find all
     the conflicts in the board (similar logic/workflow as main.ml)*)
  let all_conflicts board =
    let n = Array.length board in
    let conflict_set = ref [] in

    for r = 0 to n - 1 do
      for c = 0 to n - 1 do
        (* Only check non-empty cells *)
        match board.(r).(c) with
        | Empty -> ()
        | Initial v | UserInput v ->
            let conflicts = find_conflicts board r c v in
            (* Add (r,c) itself if it has any conflicts *)
            let conflicts =
              if conflicts <> [] then (r, c) :: conflicts else conflicts
            in
            (* Insert into global list without duplicates *)
            List.iter
              (fun pos ->
                if not (List.mem pos !conflict_set) then
                  conflict_set := pos :: !conflict_set)
              conflicts
      done
    done;

    !conflict_set
  in

  let conflicts = all_conflicts board in
  let actual = string_of_board_with_conflicts board conflicts in
  let expected =
    horizontal
    (* row 0: UserInput 1 at (0,0) in conflict → RED *)
    (*          Empty      Initial 1 (conflict → ORANGE)  Empty *)
    ^ "|"
    ^ red ^ "1" ^ reset ^ " . |" ^ orange ^ "1" ^ reset ^ " .|\n"
    (* row 1 *)
    ^ "|. "
    ^ blue ^ "3" ^ reset ^ " |. .|\n"
    (* row 2: conflict at (2,1) *)
    ^ horizontal
    ^ "|. " ^ blue ^ "1" ^ reset ^ " |. " ^ blue ^ "4" ^ reset ^ "|\n"
    (* row 3 *)
    ^ "|"
    ^ blue ^ "2" ^ reset ^ " . |. .|\n" ^ horizontal
  in

  assert_equal ~printer:(fun x -> x) expected actual

let test_board_complete_false _ =
  let board =
    [|
      [| Initial 1; Initial 2; Empty; Initial 4 |];
      [| Initial 3; Initial 4; Initial 1; Initial 2 |];
      [| Initial 2; Initial 1; Initial 4; Initial 3 |];
      [| Initial 4; Initial 3; Initial 2; Initial 1 |];
    |]
  in
  assert_equal false (is_board_complete board)

let test_board_complete_true _ =
  let board =
    [|
      [| Initial 1; Initial 2; Initial 3; Initial 4 |];
      [| Initial 3; Initial 4; Initial 1; Initial 2 |];
      [| Initial 2; Initial 1; Initial 4; Initial 3 |];
      [| Initial 4; Initial 3; Initial 2; Initial 1 |];
    |]
  in
  assert_equal true (is_board_complete board)

let test_board_valid_true _ =
  (* A correct 4x4 Sudoku *)
  let board =
    [|
      [| Initial 1; Initial 2; Initial 3; Initial 4 |];
      [| Initial 3; Initial 4; Initial 1; Initial 2 |];
      [| Initial 2; Initial 1; Initial 4; Initial 3 |];
      [| Initial 4; Initial 3; Initial 2; Initial 1 |];
    |]
  in
  assert_equal true (is_board_valid board)

let test_board_valid_false_row_conflict _ =
  (* Row 0 has two 1's *)
  let board =
    [|
      [| Initial 1; Initial 1; Initial 3; Initial 4 |];
      [| Initial 3; Initial 4; Initial 1; Initial 2 |];
      [| Initial 2; Empty; Initial 4; Initial 3 |];
      [| Initial 4; Initial 3; Initial 2; Initial 1 |];
    |]
  in
  assert_equal false (is_board_valid board)

let test_board_valid_false_col_conflict _ =
  (* Column 0 has two 1’s *)
  let board =
    [|
      [| Initial 1; Initial 2; Initial 3; Initial 4 |];
      [| Initial 3; Initial 4; Initial 1; Initial 2 |];
      [| Initial 1; Empty; Initial 4; Initial 3 |];
      [| Initial 4; Initial 3; Initial 2; Initial 1 |];
    |]
  in
  assert_equal false (is_board_valid board)

let test_board_valid_false_box_conflict _ =
  (* Top-left 2×2 box has two 1’s *)
  let board =
    [|
      [| Initial 1; Initial 2; Initial 3; Initial 4 |];
      [| Initial 1; Initial 4; Empty; Initial 2 |];
      [| Initial 2; Initial 1; Initial 4; Initial 3 |];
      [| Initial 4; Initial 3; Initial 2; Initial 1 |];
    |]
  in
  assert_equal false (is_board_valid board)

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
         "starts_with" >:: test_starts_with;
         "ends_with" >:: test_ends_with;
         "number_in_range" >:: test_number_in_range;
         "randomness_variety" >:: test_randomness_produces_variety;
         "all 16x16 CSV board tests"
         >::: List.flatten (List.map make_tests_for_file all_paths);
         "row conflict" >:: test_row_conflict;
         "column conflict" >:: test_column_conflict;
         "box conflict" >:: test_box_conflict;
         "valid move" >:: test_valid_move;
         "row_conflict" >:: test_row_conflicts;
         "col_conflict" >:: test_col_conflicts;
         "box_conflict" >:: test_box_conflicts;
         "no_duplicates" >:: test_no_conflicts;
         "string_of_board_with_conflicts"
         >:: test_string_of_board_with_conflicts;
         "string_of_board_with_conflicts main.ml workflow"
         >:: test_conflict_workflow;
         "complete_false" >:: test_board_complete_false;
         "complete_true" >:: test_board_complete_true;
         "valid_true" >:: test_board_valid_true;
         "invalid_row" >:: test_board_valid_false_row_conflict;
         "invalid_col" >:: test_board_valid_false_col_conflict;
         "invalid_box" >:: test_board_valid_false_box_conflict;
       ]

let () = run_test_tt_main suite
