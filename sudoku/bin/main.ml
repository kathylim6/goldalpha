open Sudoku.Sudokulogic

let () =
  let inital_walkthrough__message = "Let's say we start out with this board:" in
  let initial_example_board =
    [|
      [| Empty; Empty; Initial 1; Initial 2 |];
      [| Initial 2; Empty; Empty; Initial 3 |];
      [| Initial 3; Empty; Initial 2; Empty |];
      [| Empty; Initial 2; Empty; Initial 4 |];
    |]
  in

  let string_initial_board = string_of_board initial_example_board in
  let second_walkthrough_message =
    "In order to make a change, we can type a command in the format r c v, \
     where r is the row of the cell want to change, c is the column, and v is \
     the value we want to update the board with."
  in
  let third_walkthrough_message =
    "Let's say we provide the command 0 0 4. Our board would update as follows:"
  in

  let example_board_correct_change =
    [|
      [| UserInput 4; Empty; Initial 1; Initial 2 |];
      [| Initial 2; Empty; Empty; Initial 3 |];
      [| Initial 3; Empty; Initial 2; Empty |];
      [| Empty; Initial 2; Empty; Initial 4 |];
    |]
  in
  let string_board_correct_change =
    string_of_board example_board_correct_change
  in
  let fourth_walkthrough_message =
    "Now, let's say that we provide a command that violates the rules of \
     sudoku. For instance, 0 1 4. The game would indicate that this input \
     violates the rules of sudoku and reprompt, but let's understand why. Here \
     is the board with this invalid change:"
  in
  let example_board_incorrect_change =
    [|
      [| UserInput 4; UserInput 4; Initial 1; Initial 2 |];
      [| Initial 2; Empty; Empty; Initial 3 |];
      [| Initial 3; Empty; Initial 2; Empty |];
      [| Empty; Initial 2; Empty; Initial 4 |];
    |]
  in
  let string_board_incorrect_change =
    string_of_board example_board_incorrect_change
  in
  let fifth_walkthrough_message =
    "Clearly, the first row is invalid since every number can only appear once \
     in a given row, and four appears twice. When solving the board, the game \
     will not allow you to make invalid changes like these. Now, let's take a \
     look at a correct solution for the board:"
  in
  let example_final_board =
    [|
      [| UserInput 4; UserInput 3; Initial 1; Initial 2 |];
      [| Initial 2; UserInput 1; UserInput 4; Initial 3 |];
      [| Initial 3; UserInput 4; Initial 2; UserInput 1 |];
      [| UserInput 1; Initial 2; UserInput 3; Initial 4 |];
    |]
  in
  let string_final_board = string_of_board example_final_board in
  let sixth_walkthrough_message =
    "As can be seen, every box, row, and column contains each number from 1 to \
     4 exactly once. This is a correct solution. If you reach a correct \
     solution like this, then you win the game."
  in

  let walkthrough_strings =
    [
      inital_walkthrough__message;
      string_initial_board;
      second_walkthrough_message;
      third_walkthrough_message;
      string_board_correct_change;
      fourth_walkthrough_message;
      string_board_incorrect_change;
      fifth_walkthrough_message;
      string_final_board;
      sixth_walkthrough_message;
    ]
  in

  let rule_one =
    "-> Sudoku is a widely popular logic puzzle in which one attempts to fill \
     in a grid using a certain subset of numbers."
  in
  let rule_two =
    "-> If you chose to play with a 4x4 board, the numbers you are allowed to \
     use are 1-4."
  in
  let rule_three =
    "-> If you chose to play with a 9x9 board, the numbers you are allowed to \
     use are 1-9."
  in
  let rule_four =
    "-> If you chose to play with a 16x16 board, the numbers you are allowed \
     to use are 1-16."
  in
  let rule_five =
    "-> When the game starts, some cells will already be filled with fixed \
     values."
  in
  let rule_six = "-> These starting values can not be changed." in
  let rule_seven =
    "-> Your goal is to fill in the remaining cells in a manner such each row, \
     column, and box contains all of the numbers in the subset exactly once."
  in
  let sudoku_rules =
    [
      rule_one; rule_two; rule_three; rule_four; rule_five; rule_six; rule_seven;
    ]
  in
  print_endline
    "Hello, welcome to Sudoku! Would you like to review the rules or a \
     walkthrough? Respond with (R) for rules, (W) for a walkthrough, and \
     anything else for no.";
  let initial_response = read_line () in
  print_newline ();
  if initial_response = "R" || initial_response = "r" then
    let rules_length = List.length sudoku_rules in
    for i = 0 to rules_length - 1 do
      print_endline (List.nth sudoku_rules i)
    done
  else if initial_response = "W" || initial_response = "w" then
    let walkthrough_length = List.length walkthrough_strings in
    for i = 0 to walkthrough_length - 1 do
      print_endline (List.nth walkthrough_strings i)
    done
  else print_endline "Great, let's get right to it.";
  print_newline ();
  print_endline
    "Would you like to solve a 4x4 board, a 9x9 board, or a 16x16 board? \
     Respond with (4), (9), or (16).";
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

  print_newline ();
  print_endline "Here is your board:";
  print_endline (string_of_board board);

  let full_history = ref [] in
  let valid_history = ref [] in

  let playing = ref true in
  while !playing do
    print_endline "\nEnter row, column, and value (e.g., '0 1 5') or 'quit':";
    let input = read_line () in
    if input = "quit" then playing := false
    else full_history := input :: !full_history;
    try
      let parts = String.split_on_char ' ' input in
      match parts with
      | [ row_str; col_str; val_str ] ->
          let row = int_of_string row_str in
          let col = int_of_string col_str in
          let value = int_of_string val_str in

          if value < 1 || value > chosen_int then
            print_endline
              ("Error: Value must be between 1 and " ^ string_of_int chosen_int
             ^ "!")
          else if
            match board.(row).(col) with
            | Initial _ -> true
            | _ -> false
          then print_endline "Error: Cannot modify an initial cell!"
          else if check_invalid_input value row col board then
            print_endline "Error: This input violates the rules of Sudoku!"
          else (
            valid_history := input :: !valid_history;
            board.(row).(col) <- UserInput value;
            print_endline "\nUpdated board:";
            print_endline (string_of_board board))
      | _ -> print_endline "Invalid input format! Use: row col value"
    with
    | Invalid_argument _ -> print_endline "Invalid number format!"
    | _ -> print_endline "Invalid input! Make sure row/col are within bounds."
  done;
  print_endline "Thanks for playing!"
