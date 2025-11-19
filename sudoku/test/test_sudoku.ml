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

let test_generate _ =
  assert_equal four_output (generate_board 4);

  assert_equal
    "---------------------\n\
     |. . 4 |. 1 . |9 8 .|\n\
     |. . . |. 9 . |. . .|\n\
     |. . . |2 7 . |. . .|\n\
     ---------------------\n\
     |. . . |5 . . |. 1 .|\n\
     |. 7 . |. . 3 |. . 4|\n\
     |. 9 . |. . . |2 . .|\n\
     ---------------------\n\
     |. 1 . |6 . 5 |. 4 .|\n\
     |7 . . |. 3 . |. . .|\n\
     |6 . . |. . . |5 9 .|\n\
     ---------------------\n"
    (generate_board 9);
  assert_equal
    "----------------------------------------------------\n\
     |.  8  .  .  |.  2  16 .  |14 .  11 7  |10 .  4  . |\n\
     |.  15 .  .  |.  8  .  13 |.  .  9  .  |.  16 14 . |\n\
     |14 .  .  16 |.  11 12 8  |6  2  10 13 |5  .  1  . |\n\
     |.  .  11 .  |.  .  .  16 |7  15 .  .  |.  .  .  . |\n\
     ----------------------------------------------------\n\
     |6  .  15 .  |.  14 7  .  |.  4  .  .  |.  10 13 . |\n\
     |.  2  .  .  |1  .  15 13 |3  .  .  16 |5  11 .  . |\n\
     |5  .  3  .  |.  .  .  15 |8  6  .  .  |.  14 .  . |\n\
     |.  .  16 7  |3  .  5  .  |.  .  10 2  |4  .  .  . |\n\
     ----------------------------------------------------\n\
     |.  6  .  3  |14 9  .  .  |.  .  .  .  |1  .  .  . |\n\
     |10 8  4  .  |.  1  .  .  |2  .  .  .  |12 15 .  16|\n\
     |9  1  5  .  |.  .  .  10 |.  .  7  .  |.  3  13 12|\n\
     |16 .  12 .  |.  .  6  11 |.  1  .  13 |9  2  8  10|\n\
     ----------------------------------------------------\n\
     |.  3  .  .  |.  2  1  4  |.  15 .  .  |.  .  11 5 |\n\
     |11 .  13 .  |6  .  .  5  |.  .  4  .  |3  .  8  . |\n\
     |15 4  .  .  |.  .  3  .  |.  2  .  .  |.  .  .  . |\n\
     |.  14 .  .  |.  10 15 7  |5  9  .  .  |12 13 .  3 |\n\
     ----------------------------------------------------\n"
    (generate_board 16)

(** Test Suite for Sudoku Functions *)
let suite =
  "SudokuTests"
  >::: [
         "string_of_cell" >:: test_cell;
         "string_of_row" >:: test_row;
         "string_of_board" >:: test_board;
         "generate_board" >:: test_generate;
       ]

let () = run_test_tt_main suite
