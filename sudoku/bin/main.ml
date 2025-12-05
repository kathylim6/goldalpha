open Sudoku.Sudokulogic

let red = "\027[31m"
let green = "\027[32m"
let reset = "\027[0m"

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
     violates the rules of sudoku and not save the change, but let's \
     understand why. Here is the board with this invalid change:"
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

  if chosen_int = 4 || chosen_int = 9 then
    print_endline
      "If select whether the level you want to play \n\n\
      \      Level 1 Easy : (1) \n\n\
      \      Level 2 Medium : (2) \n\n\
      \      Level 3 Hard : (3) \n\
      \      ";

  let pre_processed_level = read_line () in
  let level = int_of_string pre_processed_level in

  print_endline ("You are playing at level " ^ pre_processed_level);

  let board =
    match chosen_int with
    | 4 -> generate_board 4 level
    | 9 -> generate_board 9 level
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
    print_endline "\nEnter row, column, and value (e.g., '0 1 5')";
    print_endline "Enter '0' as the value to delete a cell, or 'quit' to exit:";
    let input = read_line () in
    if input = "quit" then playing := false
    else begin
      full_history := input :: !full_history;
      try
        let parts = String.split_on_char ' ' input in
        match parts with
        | [ row_str; col_str; val_str ] ->
            let row = int_of_string row_str in
            let col = int_of_string col_str in
            let value = int_of_string val_str in

            if
              match board.(row).(col) with
              | Initial _ -> true
              | _ -> false
            then
              print_endline
                (red ^ "Error: Cannot modify an initial cell!" ^ reset)
            else if value = 0 then
              match board.(row).(col) with
              | UserInput _ ->
                  board.(row).(col) <- Empty;
                  valid_history := input :: !valid_history;
                  print_endline
                    (green ^ "\nCell cleared! Updated board:" ^ reset);
                  print_endline (string_of_board board)
              | Empty -> print_endline "Cell is already empty!"
              | Initial _ ->
                  print_endline
                    (red ^ "Error: Cannot delete an initial cell!" ^ reset)
            else if value < 1 || value > chosen_int then
              print_endline
                (red ^ "Error: Value must be between 1 and "
               ^ string_of_int chosen_int ^ ", or 0 to delete!" ^ reset)
            else if check_invalid_input value row col board then (
              let temp_board =
                Array.init (Array.length board) (fun i -> Array.copy board.(i))
              in
              temp_board.(row).(col) <- UserInput value;

              let conflicts = find_conflicts temp_board row col value in
              let all_conflicts = (row, col) :: conflicts in

              print_endline
                (red ^ "This input violates the rules of Sudoku!" ^ reset);
              print_endline "What you attempted:";
              print_endline
                (string_of_board_with_conflicts temp_board all_conflicts))
            else (
              valid_history := input :: !valid_history;
              board.(row).(col) <- UserInput value;
              print_endline (green ^ "\nValid move! Updated board:" ^ reset);
              print_endline (string_of_board board);

              if is_board_complete board then
                if is_board_valid board then (
                  print_endline "";
                  print_endline
                    (green ^ "╔═══════════════════════════════════════╗" ^ reset);
                  print_endline
                    (green ^ "║                                       ║" ^ reset);
                  print_endline
                    (green ^ "║       CONGRATULATIONS! YOU WON!       ║" ^ reset);
                  print_endline
                    (green ^ "║                                       ║" ^ reset);
                  print_endline
                    (green ^ "║  You successfully solved the puzzle!  ║" ^ reset);
                  print_endline
                    (green ^ "║                                       ║" ^ reset);
                  print_endline
                    (green ^ "╚═══════════════════════════════════════╝" ^ reset);
                  print_endline "";
                  playing := false)
                else
                  print_endline
                    (red ^ "Board is full but contains errors. Keep trying!"
                   ^ reset))
        | _ ->
            print_endline
              (red ^ "Invalid input format! Use: row col value" ^ reset)
      with
      | Invalid_argument _ ->
          print_endline (red ^ "Invalid number format!" ^ reset)
      | _ ->
          print_endline
            (red ^ "Invalid input! Make sure row/col are within bounds." ^ reset)
    end
  done;
  print_endline
    "Thanks for playing! Would you like to see your valid move history or full \
     move history? Respond with (V) for valid move history, (F) with full move \
     history, and anything else for neither.";
  let history_reponse = read_line () in
  if history_reponse = "V" || history_reponse = "v" then
    let reversed_history = List.rev !valid_history in
    let history_length = List.length reversed_history in
    if history_length = 0 then print_endline "You haven't made any valid moves."
    else
      for i = 0 to history_length - 1 do
        print_endline (List.nth reversed_history i)
      done
  else if history_reponse = "F" || history_reponse = "f" then
    let reversed_history = List.rev !full_history in
    let history_length = List.length reversed_history in
    if history_length = 0 then print_endline "You haven't made any moves."
    else
      for i = 0 to history_length - 1 do
        print_endline (List.nth reversed_history i)
      done
  else print_endline "Have a nice rest of your day!"
