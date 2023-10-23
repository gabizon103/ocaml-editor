(** Defines a way to parse user keypresses. *)

(** [input] represents user input parsed into a form that we can work with. *)
type input =
  | Empty
  | Tab
  | Esc
  | Char of char
  | Ctrl of char
  | Sel of input
  | Backspace
  | Enter
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight

val read_input : unit -> input
(** [read_input ()] receives a keypress from the input channel and parses it
    into type [input]. *)
