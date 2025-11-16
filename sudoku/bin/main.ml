open Sudoku.Sudokulogic

let () =
  print_endline
    "Hello, welcome to Sudoku! Would you like to solve a 4x4 board, a 9x9 board, \
     or a 16x16 board? Respond with (4), (9), or (16).";
  let chosen_size = ref (read_line ()) in
  while !chosen_size <> "4" && !chosen_size <> "9" && !chosen_size <> "16" do
    print_endline "That wasn't a valid input, try again. Respond with (4), (9), or (16).";
    chosen_size := read_line ()
  done;
  let chosen_int = int_of_string !chosen_size in
  print_endline ("Here is your board:");
  print_endline (generate_board chosen_int)



