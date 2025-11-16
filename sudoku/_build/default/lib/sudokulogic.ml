let four_board = 
  [|
    [| Some 1; None;   None;   Some 4 |];
    [| None;  Some 3; None;   None   |];
    [| None;  None;   Some 2; None   |];
    [| Some 4; None;  None;   Some 1 |];
  |]

let nine_board =
      [|
      [| Some 1; None;   None;   None;   Some 2; None;   None;   None;   Some 3 |];
      [| None;   Some 4; None;   None;   None;   Some 5; None;   None;   None   |];
      [| None;   None;   Some 6; Some 7; None;   None;   Some 8; None;   None   |];
      [| None;   None;   Some 1; None;   None;   None;   None;   Some 9; None   |];
      [| Some 2; None;   None;   None;   Some 3; None;   None;   None;   Some 4 |];
      [| None;   Some 5; None;   None;   None;   Some 6; None;   None;   None   |];
      [| None;   None;   Some 7; None;   None;   None;   Some 1; None;   None   |];
      [| None;   None;   None;   Some 8; None;   None;   None;   Some 2; None   |];
      [| Some 3; None;   None;   None;   Some 4; None;   None;   None;   Some 5 |];
    |]

let sixteen_board =
    [|
      [| Some 1;  None;    None;    None;    None;    None;    None;    None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    Some 2;  None;    None;    None;    None;    None;    None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    Some 3;  None;    None;    None;    None;    None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    Some 4;  None;    None;    None;    None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    Some 5;  None;    None;    None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    Some 6;  None;    None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    Some 7;  None;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    Some 8;
         None;    None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         Some 9;  None;    None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    Some 10; None;    None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    None;    Some 11; None;    None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    None;    None;    Some 12; None;    None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    None;    None;    None;    Some 13; None;    None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    None;    None;    None;    None;    Some 14; None;    None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    None;    None;    None;    None;    None;    Some 15; None |];
      [| None;    None;    None;    None;    None;    None;    None;    None;
         None;    None;    None;    None;    None;    None;    None;    Some 16 |];
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