(** add spec*)

(** A cell in a Sudoku board. *)
type cell =
  | Initial of int
    (* A fixed, pre-filled value that is part of the puzzle’s initial state. *)
  | Empty (* An empty cell with no value assigned. *)
  | UserInput of int  (** A value supplied by the user during gameplay. *)
