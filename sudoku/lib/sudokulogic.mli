type cell =
  | Initial of int
  | Empty
  | UserInput of int

val four_board : cell array array
val nine_board : cell array array
val sixteen_board : cell array array
val string_of_cell : cell -> string
val string_of_row : cell array -> int -> string
val string_of_board : cell array array -> string
val generate_board : int -> string
