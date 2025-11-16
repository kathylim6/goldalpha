let four_board = 
  [|
    [| Some 1; None;   None;   Some 4 |];
    [| None;  Some 3; None;   None   |];
    [| None;  None;   Some 2; None   |];
    [| Some 4; None;  None;   Some 1 |];
  |]

let nine_board =
  [|
    [| Some 1; None;   Some 3; None;   Some 5; None;   Some 7; None;   Some 9 |];
    [| None;   Some 5; None;   Some 7; None;   Some 9; None;   Some 2; None   |];
    [| Some 7; None;   Some 9; None;   Some 2; None;   Some 4; None;   Some 6 |];
    [| None;   Some 3; None;   Some 5; None;   Some 7; None;   Some 9; None   |];
    [| Some 5; None;   Some 7; None;   Some 9; None;   Some 2; None;   Some 4 |];
    [| None;   Some 9; None;   Some 2; None;   Some 4; None;   Some 6; None   |];
    [| Some 3; None;   Some 5; None;   Some 7; None;   Some 9; None;   Some 2 |];
    [| None;   Some 7; None;   Some 9; None;   Some 2; None;   Some 4; None   |];
    [| Some 9; None;   Some 2; None;   Some 4; None;   Some 6; None;   Some 8 |];
  |]

let sixteen_board =
  [|
    [| Some 1;  None;   Some 3;  None;   Some 5;  None;   Some 7;  None;
       Some 9;  None;   Some 11; None;   Some 13; None;   Some 15; None |];
    [| None;    Some 6; None;   Some 8;  None;   Some 10; None;   Some 12;
       None;    Some 14; None;   Some 16; None;   Some 2;  None;   Some 4  |];
    [| Some 9;  None;   Some 11; None;   Some 13; None;   Some 15; None;
       Some 1;  None;   Some 3;  None;   Some 5;  None;   Some 7;  None |];
    [| None;    Some 14; None;   Some 16; None;   Some 2;  None;   Some 4;
       None;    Some 6;  None;   Some 8;  None;   Some 10; None;   Some 12 |];

    [| Some 2;  None;   Some 4;  None;   Some 6;  None;   Some 8;  None;
       Some 10; None;   Some 12; None;   Some 14; None;   Some 16; None |];
    [| None;    Some 7; None;   Some 9;  None;   Some 11; None;   Some 13;
       None;    Some 15; None;   Some 1;  None;   Some 3;  None;   Some 5  |];
    [| Some 10; None;   Some 12; None;   Some 14; None;   Some 16; None;
       Some 2;  None;   Some 4;  None;   Some 6;  None;   Some 8;  None |];
    [| None;    Some 15; None;   Some 1;  None;   Some 3;  None;   Some 5;
       None;    Some 7;  None;   Some 9;  None;   Some 11; None;   Some 13 |];

    [| Some 3;  None;   Some 5;  None;   Some 7;  None;   Some 9;  None;
       Some 11; None;   Some 13; None;   Some 15; None;   Some 1;  None |];
    [| None;    Some 8; None;   Some 10; None;   Some 12; None;   Some 14;
       None;    Some 16; None;   Some 2;  None;   Some 4;  None;   Some 6  |];
    [| Some 11; None;   Some 13; None;   Some 15; None;   Some 1;  None;
       Some 3;  None;   Some 5;  None;   Some 7;  None;   Some 9;  None |];
    [| None;    Some 16; None;   Some 2;  None;   Some 4;  None;   Some 6;
       None;    Some 8;  None;   Some 10; None;   Some 12; None;   Some 14 |];

    [| Some 4;  None;   Some 6;  None;   Some 8;  None;   Some 10; None;
       Some 12; None;   Some 14; None;   Some 16; None;   Some 2;  None |];
    [| None;    Some 9; None;   Some 11; None;   Some 13; None;   Some 15;
       None;    Some 1;  None;   Some 3;  None;   Some 5;  None;   Some 7  |];
    [| Some 12; None;   Some 14; None;   Some 16; None;   Some 2;  None;
       Some 4;  None;   Some 6;  None;   Some 8;  None;   Some 10; None |];
    [| None;    Some 1; None;   Some 3;  None;   Some 5;  None;   Some 7;
       None;    Some 9;  None;   Some 11; None;   Some 13; None;   Some 15 |];
  |]

let string_of_box = function
  | None -> "."
  | Some v -> string_of_int v

let string_of_row input_row =
  input_row
  |> Array.to_list
  |> List.map string_of_box
  |> String.concat " "

let string_of_board input_board : string =
  input_board
  |> Array.to_list
  |> List.map string_of_row
  |> String.concat "\n"

let generate_board num = 
  match num with
  | 4 -> string_of_board four_board
  | 9 -> string_of_board nine_board
  | _ -> string_of_board sixteen_board