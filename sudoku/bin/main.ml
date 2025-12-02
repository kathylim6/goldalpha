open Sudoku.Sudokulogic

let () =
  print_endline
    "Hello, welcome to Sudoku! Would you like to solve a 4x4 board, a 9x9 \
     board, or a 16x16 board? Respond with (4), (9), or (16).";
  let chosen_size = ref (read_line ()) in
  while !chosen_size <> "4" && !chosen_size <> "9" && !chosen_size <> "16" do
    print_endline
      "That wasn't a valid input, try again. Respond with (4), (9), or (16).";
    chosen_size := read_line ()
  done;
  let chosen_int = int_of_string !chosen_size in

  let board =
    match chosen_int with
    | 4 -> generate_board 4
    | 9 -> generate_board 9
    | 16 -> make_sixteen_board (choose_random_file_path ())
    | _ -> failwith "Only 4x4, 9x9, or 16x16 boards supported"
  in

  print_endline "Here is your board:";
  print_endline (string_of_board board);

  let playing = ref true in
  while !playing do
    print_endline "\nEnter row, column, and value (e.g., '0 1 5') or 'quit':";
    let input = read_line () in
    if input = "quit" then playing := false
    else
      try
        let parts = String.split_on_char ' ' input in
        match parts with
        | [ row_str; col_str; val_str ] -> (
            let row = int_of_string row_str in
            let col = int_of_string col_str in
            let value = int_of_string val_str in
            if value < 1 || value > chosen_int then
              print_endline
                ("Error: Value must be between 1 and "
               ^ string_of_int chosen_int ^ "!")
            else
              match board.(row).(col) with
              | Initial _ ->
                  print_endline "Error: Cannot modify an initial cell!"
              | Empty | UserInput _ ->
                  board.(row).(col) <- UserInput value;
                  print_endline "\nUpdated board:";
                  print_endline (string_of_board board))
        | _ -> print_endline "Invalid input format! Use: row col value"
      with
      | Invalid_argument _ -> print_endline "Invalid number format!"
      | _ -> print_endline "Invalid input! Make sure row/col are within bounds."
  done;
  print_endline "Thanks for playing!"
