open OUnit2
open Sudoku.Sudokulogic

let test_cell _ =
  assert_equal "." (string_of_cell Empty 1);
  assert_equal "3" (string_of_cell (Initial 3) 1);
  assert_equal "6" (string_of_cell (UserInput 6) 1);
  assert_equal "12" (string_of_cell (UserInput 12) 2)

let test_row _ =
  assert_equal "|. 1 |. 3|"
    (string_of_row [| Empty; Initial 1; Empty; Initial 3 |] 2);
  assert_equal "|. 2 3 |1 . 9"
    (string_of_row
       [| Empty; UserInput 2; Initial 3; UserInput 1; Empty; Initial 9 |]
       3)

let four_output =
  "----------\n\
   |. 3 |. 2|\n\
   |. . |4 3|\n\
   ----------\n\
   |2 . |3 .|\n\
   |3 4 |. .|\n\
   ----------\n"

let test_board _ = assert_equal four_output (string_of_board four_board)

(* let test_generate _ = assert_equal four_output (generate_board 4);

   assert_equal "---------------------\n\ |. . 4 |. 1 . |9 8 .|\n\ |. . . |. 9 .
   |. . .|\n\ |. . . |2 7 . |. . .|\n\ ---------------------\n\ |. . . |5 . . |.
   1 .|\n\ |. 7 . |. . 3 |. . 4|\n\ |. 9 . |. . . |2 . .|\n\
   ---------------------\n\ |. 1 . |6 . 5 |. 4 .|\n\ |7 . . |. 3 . |. . .|\n\ |6
   . . |. . . |5 9 .|\n\ ---------------------\n" (generate_board 9);
   assert_equal "----------------------------------------------------\n\ |. 8 .
   . |. 2 16 . |14 . 11 7 |10 . 4 . |\n\ |. 15 . . |. 8 . 13 |. . 9 . |. 16 14 .
   |\n\ |14 . . 16 |. 11 12 8 |6 2 10 13 |5 . 1 . |\n\ |. . 11 . |. . . 16 |7 15
   . . |. . . . |\n\ ----------------------------------------------------\n\ |6
   . 15 . |. 14 7 . |. 4 . . |. 10 13 . |\n\ |. 2 . . |1 . 15 13 |3 . . 16 |5 11
   . . |\n\ |5 . 3 . |. . . 15 |8 6 . . |. 14 . . |\n\ |. . 16 7 |3 . 5 . |. .
   10 2 |4 . . . |\n\ ----------------------------------------------------\n\ |.
   6 . 3 |14 9 . . |. . . . |1 . . . |\n\ |10 8 4 . |. 1 . . |2 . . . |12 15 .
   16|\n\ |9 1 5 . |. . . 10 |. . 7 . |. 3 13 12|\n\ |16 . 12 . |. . 6 11 |. 1 .
   13 |9 2 8 10|\n\ ----------------------------------------------------\n\ |. 3
   . . |. 2 1 4 |. 15 . . |. . 11 5 |\n\ |11 . 13 . |6 . . 5 |. . 4 . |3 . 8 .
   |\n\ |15 4 . . |. . 3 . |. 2 . . |. . . . |\n\ |. 14 . . |. 10 15 7 |5 9 . .
   |12 13 . 3 |\n\ ----------------------------------------------------\n"
   (generate_board 16) *)

(** Test Suite for Sudoku Functions *)
let suite =
  "SudokuTests"
  >::: [
         "string_of_cell" >:: test_cell;
         "string_of_row" >:: test_row;
         "string_of_board" >:: test_board;
         ( "make_four_board: board is 4x4" >:: fun _ ->
           let board = make_four_board () in
           let num_rows = Array.length board in
           let check_cols = ref true in
           let row_index = ref 0 in
           while !row_index < num_rows do
             if Array.length board.(!row_index) <> 4 then check_cols := false;
             row_index := !row_index + 1
           done;
           let equality = num_rows = 4 && !check_cols in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_four_board: all cells are Initial" >:: fun _ ->
           let board = make_four_board () in
           let allowed_cells = ref [] in
           let value = ref 1 in
           while !value <= 4 do
             allowed_cells := Initial !value :: !allowed_cells;
             value := !value + 1
           done;
           let all_initial = ref true in
           let row_index = ref 0 in
           while !row_index < 4 do
             let col_index = ref 0 in
             while !col_index < 4 do
               let current_cell = board.(!row_index).(!col_index) in
               if not (List.mem current_cell !allowed_cells) then
                 all_initial := false;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !all_initial in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_four_board: all values in 1..4" >:: fun _ ->
           let board = make_four_board () in
           let allowed_cells = ref [] in
           let value = ref 1 in
           while !value <= 4 do
             allowed_cells := Initial !value :: !allowed_cells;
             value := !value + 1
           done;
           let all_in_range = ref true in
           let row_index = ref 0 in
           while !row_index < 4 do
             let col_index = ref 0 in
             while !col_index < 4 do
               let current_cell = board.(!row_index).(!col_index) in
               if not (List.mem current_cell !allowed_cells) then
                 all_in_range := false;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !all_in_range in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_four_board: each row has distinct values" >:: fun _ ->
           let board = make_four_board () in
           let check_distinct = ref true in
           let row_index = ref 0 in
           while !row_index < 4 do
             let seen_cells = ref [] in
             let col_index = ref 0 in
             while !col_index < 4 do
               let current_cell = board.(!row_index).(!col_index) in
               if List.mem current_cell !seen_cells then check_distinct := false
               else seen_cells := current_cell :: !seen_cells;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_four_board: each column has distinct values" >:: fun _ ->
           let board = make_four_board () in
           let check_distinct = ref true in
           let col_index = ref 0 in
           while !col_index < 4 do
             let seen_cells = ref [] in
             let row_index = ref 0 in
             while !row_index < 4 do
               let current_cell = board.(!row_index).(!col_index) in
               if List.mem current_cell !seen_cells then check_distinct := false
               else seen_cells := current_cell :: !seen_cells;
               row_index := !row_index + 1
             done;
             col_index := !col_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_four_board: each 2x2 box has distinct values" >:: fun _ ->
           let board = make_four_board () in
           let check_distinct = ref true in
           let box_row_index = ref 0 in
           while !box_row_index < 2 do
             let box_col_index = ref 0 in
             while !box_col_index < 2 do
               let seen_cells = ref [] in
               let box_row_position = ref 0 in
               while !box_row_position < 2 do
                 let box_col_position = ref 0 in
                 while !box_col_position < 2 do
                   let row_index = (!box_row_index * 2) + !box_row_position in
                   let col_index = (!box_col_index * 2) + !box_col_position in
                   let current_cell = board.(row_index).(col_index) in
                   if List.mem current_cell !seen_cells then
                     check_distinct := false
                   else seen_cells := current_cell :: !seen_cells;
                   box_col_position := !box_col_position + 1
                 done;
                 box_row_position := !box_row_position + 1
               done;
               box_col_index := !box_col_index + 1
             done;
             box_row_index := !box_row_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         (* ---------- 9x9 tests ---------- *)
         ( "make_nine_board: board is 9x9" >:: fun _ ->
           let board = make_nine_board () in
           let num_rows = Array.length board in
           let check_cols = ref true in
           let row_index = ref 0 in
           while !row_index < num_rows do
             if Array.length board.(!row_index) <> 9 then check_cols := false;
             row_index := !row_index + 1
           done;
           let equality = num_rows = 9 && !check_cols in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_nine_board: all cells are Initial" >:: fun _ ->
           let board = make_nine_board () in
           let allowed_cells = ref [] in
           let value = ref 1 in
           while !value <= 9 do
             allowed_cells := Initial !value :: !allowed_cells;
             value := !value + 1
           done;
           let all_initial = ref true in
           let row_index = ref 0 in
           while !row_index < 9 do
             let col_index = ref 0 in
             while !col_index < 9 do
               let current_cell = board.(!row_index).(!col_index) in
               if not (List.mem current_cell !allowed_cells) then
                 all_initial := false;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !all_initial in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_nine_board: all values in 1..9" >:: fun _ ->
           let board = make_nine_board () in
           let allowed_cells = ref [] in
           let value = ref 1 in
           while !value <= 9 do
             allowed_cells := Initial !value :: !allowed_cells;
             value := !value + 1
           done;
           let all_in_range = ref true in
           let row_index = ref 0 in
           while !row_index < 9 do
             let col_index = ref 0 in
             while !col_index < 9 do
               let current_cell = board.(!row_index).(!col_index) in
               if not (List.mem current_cell !allowed_cells) then
                 all_in_range := false;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !all_in_range in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_nine_board: each row has distinct values" >:: fun _ ->
           let board = make_nine_board () in
           let check_distinct = ref true in
           let row_index = ref 0 in
           while !row_index < 9 do
             let seen_cells = ref [] in
             let col_index = ref 0 in
             while !col_index < 9 do
               let current_cell = board.(!row_index).(!col_index) in
               if List.mem current_cell !seen_cells then check_distinct := false
               else seen_cells := current_cell :: !seen_cells;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_nine_board: each column has distinct values" >:: fun _ ->
           let board = make_nine_board () in
           let check_distinct = ref true in
           let col_index = ref 0 in
           while !col_index < 9 do
             let seen_cells = ref [] in
             let row_index = ref 0 in
             while !row_index < 9 do
               let current_cell = board.(!row_index).(!col_index) in
               if List.mem current_cell !seen_cells then check_distinct := false
               else seen_cells := current_cell :: !seen_cells;
               row_index := !row_index + 1
             done;
             col_index := !col_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_nine_board: each 3x3 box has distinct values" >:: fun _ ->
           let board = make_nine_board () in
           let check_distinct = ref true in
           let box_row_index = ref 0 in
           while !box_row_index < 3 do
             let box_col_index = ref 0 in
             while !box_col_index < 3 do
               let seen_cells = ref [] in
               let box_row_position = ref 0 in
               while !box_row_position < 3 do
                 let box_col_position = ref 0 in
                 while !box_col_position < 3 do
                   let row_index = (!box_row_index * 3) + !box_row_position in
                   let col_index = (!box_col_index * 3) + !box_col_position in
                   let current_cell = board.(row_index).(col_index) in
                   if List.mem current_cell !seen_cells then
                     check_distinct := false
                   else seen_cells := current_cell :: !seen_cells;
                   box_col_position := !box_col_position + 1
                 done;
                 box_row_position := !box_row_position + 1
               done;
               box_col_index := !box_col_index + 1
             done;
             box_row_index := !box_row_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         (* ---------- 16x16 tests ---------- *)
         ( "make_sixteen_board: board is 16x16" >:: fun _ ->
           let board = make_sixteen_board () in
           let num_rows = Array.length board in
           let check_cols = ref true in
           let row_index = ref 0 in
           while !row_index < num_rows do
             if Array.length board.(!row_index) <> 16 then check_cols := false;
             row_index := !row_index + 1
           done;
           let equality = num_rows = 16 && !check_cols in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_sixteen_board: all cells are Initial" >:: fun _ ->
           let board = make_sixteen_board () in
           let allowed_cells = ref [] in
           let value = ref 1 in
           while !value <= 16 do
             allowed_cells := Initial !value :: !allowed_cells;
             value := !value + 1
           done;
           let all_initial = ref true in
           let row_index = ref 0 in
           while !row_index < 16 do
             let col_index = ref 0 in
             while !col_index < 16 do
               let current_cell = board.(!row_index).(!col_index) in
               if not (List.mem current_cell !allowed_cells) then
                 all_initial := false;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !all_initial in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_sixteen_board: all values in 1..16" >:: fun _ ->
           let board = make_sixteen_board () in
           let allowed_cells = ref [] in
           let value = ref 1 in
           while !value <= 16 do
             allowed_cells := Initial !value :: !allowed_cells;
             value := !value + 1
           done;
           let all_in_range = ref true in
           let row_index = ref 0 in
           while !row_index < 16 do
             let col_index = ref 0 in
             while !col_index < 16 do
               let current_cell = board.(!row_index).(!col_index) in
               if not (List.mem current_cell !allowed_cells) then
                 all_in_range := false;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !all_in_range in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_sixteen_board: each row has distinct values" >:: fun _ ->
           let board = make_sixteen_board () in
           let check_distinct = ref true in
           let row_index = ref 0 in
           while !row_index < 16 do
             let seen_cells = ref [] in
             let col_index = ref 0 in
             while !col_index < 16 do
               let current_cell = board.(!row_index).(!col_index) in
               if List.mem current_cell !seen_cells then check_distinct := false
               else seen_cells := current_cell :: !seen_cells;
               col_index := !col_index + 1
             done;
             row_index := !row_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_sixteen_board: each column has distinct values" >:: fun _ ->
           let board = make_sixteen_board () in
           let check_distinct = ref true in
           let col_index = ref 0 in
           while !col_index < 16 do
             let seen_cells = ref [] in
             let row_index = ref 0 in
             while !row_index < 16 do
               let current_cell = board.(!row_index).(!col_index) in
               if List.mem current_cell !seen_cells then check_distinct := false
               else seen_cells := current_cell :: !seen_cells;
               row_index := !row_index + 1
             done;
             col_index := !col_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
         ( "make_sixteen_board: each 4x4 box has distinct values" >:: fun _ ->
           let board = make_sixteen_board () in
           let check_distinct = ref true in
           let box_row_index = ref 0 in
           while !box_row_index < 4 do
             let box_col_index = ref 0 in
             while !box_col_index < 4 do
               let seen_cells = ref [] in
               let box_row_position = ref 0 in
               while !box_row_position < 4 do
                 let box_col_position = ref 0 in
                 while !box_col_position < 4 do
                   let row_index = (!box_row_index * 4) + !box_row_position in
                   let col_index = (!box_col_index * 4) + !box_col_position in
                   let current_cell = board.(row_index).(col_index) in
                   if List.mem current_cell !seen_cells then
                     check_distinct := false
                   else seen_cells := current_cell :: !seen_cells;
                   box_col_position := !box_col_position + 1
                 done;
                 box_row_position := !box_row_position + 1
               done;
               box_col_index := !box_col_index + 1
             done;
             box_row_index := !box_row_index + 1
           done;
           let equality = !check_distinct in
           assert_equal true equality ~printer:string_of_bool );
       ]

let () = run_test_tt_main suite
