type cell =
  | Initial of int
  | Empty
  | UserInput of int

let four_board =
  [|
    [| Empty; Initial 3; Empty; Initial 2 |];
    [| Empty; Empty; Initial 4; Initial 3 |];
    [| Initial 2; Empty; Initial 3; Empty |];
    [| Initial 3; Initial 4; Empty; Empty |];
  |]

let nine_board =
  [|
    [|
      Empty;
      Empty;
      Initial 4;
      Empty;
      Initial 1;
      Empty;
      Initial 9;
      Initial 8;
      Empty;
    |];
    [| Empty; Empty; Empty; Empty; Initial 9; Empty; Empty; Empty; Empty |];
    [| Empty; Empty; Empty; Initial 2; Initial 7; Empty; Empty; Empty; Empty |];
    [| Empty; Empty; Empty; Initial 5; Empty; Empty; Empty; Initial 1; Empty |];
    [|
      Empty; Initial 7; Empty; Empty; Empty; Initial 3; Empty; Empty; Initial 4;
    |];
    [| Empty; Initial 9; Empty; Empty; Empty; Empty; Initial 2; Empty; Empty |];
    [|
      Empty;
      Initial 1;
      Empty;
      Initial 6;
      Empty;
      Initial 5;
      Empty;
      Initial 4;
      Empty;
    |];
    [| Initial 7; Empty; Empty; Empty; Initial 3; Empty; Empty; Empty; Empty |];
    [|
      Initial 6; Empty; Empty; Empty; Empty; Empty; Initial 5; Initial 9; Empty;
    |];
  |]

let sixteen_board =
  [|
    [|
      Empty;
      Initial 8;
      Empty;
      Empty;
      Empty;
      Initial 2;
      Initial 16;
      Empty;
      Initial 14;
      Empty;
      Initial 11;
      Initial 7;
      Initial 10;
      Empty;
      Initial 4;
      Empty;
    |];
    [|
      Empty;
      Initial 15;
      Empty;
      Empty;
      Empty;
      Initial 8;
      Empty;
      Initial 13;
      Empty;
      Empty;
      Initial 9;
      Empty;
      Empty;
      Initial 16;
      Initial 14;
      Empty;
    |];
    [|
      Initial 14;
      Empty;
      Empty;
      Initial 16;
      Empty;
      Initial 11;
      Initial 12;
      Initial 8;
      Initial 6;
      Initial 2;
      Initial 10;
      Initial 13;
      Initial 5;
      Empty;
      Initial 1;
      Empty;
    |];
    [|
      Empty;
      Empty;
      Initial 11;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 16;
      Initial 7;
      Initial 15;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Initial 6;
      Empty;
      Initial 15;
      Empty;
      Empty;
      Initial 14;
      Initial 7;
      Empty;
      Empty;
      Initial 4;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 13;
      Empty;
    |];
    [|
      Empty;
      Initial 2;
      Empty;
      Empty;
      Initial 1;
      Empty;
      Initial 15;
      Initial 13;
      Initial 3;
      Empty;
      Empty;
      Initial 16;
      Initial 5;
      Initial 11;
      Empty;
      Empty;
    |];
    [|
      Initial 5;
      Empty;
      Initial 3;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 15;
      Initial 8;
      Initial 6;
      Empty;
      Empty;
      Empty;
      Initial 14;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Empty;
      Initial 16;
      Initial 7;
      Initial 3;
      Empty;
      Initial 5;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 2;
      Initial 4;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 6;
      Empty;
      Initial 3;
      Initial 14;
      Initial 9;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 1;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Initial 10;
      Initial 8;
      Initial 4;
      Empty;
      Empty;
      Initial 1;
      Empty;
      Empty;
      Initial 2;
      Empty;
      Empty;
      Empty;
      Initial 12;
      Initial 15;
      Empty;
      Initial 16;
    |];
    [|
      Initial 9;
      Initial 1;
      Initial 5;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Empty;
      Empty;
      Initial 7;
      Empty;
      Empty;
      Initial 3;
      Initial 13;
      Initial 12;
    |];
    [|
      Initial 16;
      Empty;
      Initial 12;
      Empty;
      Empty;
      Empty;
      Initial 6;
      Initial 11;
      Empty;
      Initial 1;
      Empty;
      Initial 13;
      Initial 9;
      Initial 2;
      Initial 8;
      Initial 10;
    |];
    [|
      Empty;
      Initial 3;
      Empty;
      Empty;
      Empty;
      Initial 2;
      Initial 1;
      Initial 4;
      Empty;
      Initial 15;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 11;
      Initial 5;
    |];
    [|
      Initial 11;
      Empty;
      Initial 13;
      Empty;
      Initial 6;
      Empty;
      Empty;
      Initial 5;
      Empty;
      Empty;
      Initial 4;
      Empty;
      Initial 3;
      Empty;
      Initial 8;
      Empty;
    |];
    [|
      Initial 15;
      Initial 4;
      Empty;
      Empty;
      Empty;
      Empty;
      Initial 3;
      Empty;
      Empty;
      Initial 2;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
    |];
    [|
      Empty;
      Initial 14;
      Empty;
      Empty;
      Empty;
      Initial 10;
      Initial 15;
      Initial 7;
      Initial 5;
      Initial 9;
      Empty;
      Empty;
      Initial 12;
      Initial 13;
      Empty;
      Initial 3;
    |];
  |]

let string_of_cell = function
  | Empty -> "."
  | Initial v -> string_of_int v
  | UserInput v -> string_of_int v

let string_of_row input_row root =
  (* input_row |> Array.to_list |> List.map string_of_cell |> String.concat "
     " *)
  let row_list = Array.to_list input_row in
  String.concat " "
    (List.mapi
       (fun i x ->
         if i mod root = 0 then "|" ^ string_of_cell x
         else if i = (root * root) - 1 then string_of_cell x ^ "|"
         else string_of_cell x)
       row_list)

let repeat_string s n = String.concat "" (List.init n (fun _ -> s))

let string_of_board input_board : string =
  let board_size = float_of_int (Array.length input_board) in
  let root = int_of_float (sqrt board_size) in
  let board_list = Array.to_list input_board in
  String.concat ""
    (List.mapi
       (fun i x ->
         if i mod root = 0 then
           repeat_string "-" ((int_of_float board_size * 2) + root)
           ^ "\n" ^ string_of_row x root ^ "\n"
         else if i = (root * root) - 1 then
           string_of_row x root ^ "\n"
           ^ repeat_string "-" ((int_of_float board_size * 2) + root)
           ^ "\n"
         else string_of_row x root ^ "\n")
       board_list)

let generate_board num =
  match num with
  | 4 -> string_of_board four_board
  | 9 -> string_of_board nine_board
  | _ -> string_of_board sixteen_board
