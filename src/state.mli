(** Represents the overall state of the editor, as well as the operations on it. *)

(** A type representing what mode the editor is in, which will change the
    commands available to the user. *)
type mode =
  | Insert
  | View
  | Command

type t = {
  mutable rows : int option;
  mutable cols : int option;
  mutable cursor_x : int;
  mutable cursor_y : int;
  mutable current_line : Dlinkedlist.node;
  mutable file : string;
  mutable mode : mode;
  mutable clipboard : string;
  mutable selected : string;
  mutable num_lines : int;
  mutable top_line_index : int;
  mutable text_effect : ANSITerminal.style option;
}
(** A type representing the editor, containing all useful information about it. *)

(** A direction to move the cursor in. *)
type direction =
  | Up
  | Down
  | Left
  | Right

val create_state : unit -> t
(** [create_state ()] creates a new, default state of the editor. *)

val process_input : t -> unit
(** [process_input t] processes user inputs and calls other operations on [t]
    based on the input. *)

val change_dll : t -> Dlinkedlist.node -> unit
(** [change_dll t node] mutates the [current_line] field of [t] so that it
    stores [node], also adjusts the [num_lines] field to be the length of the
    DLL that [node] is part of, and sets the [cursor_x] field of t to be at the
    beginning of the line. *)

val move_cursor : t -> direction -> unit
(** [move_cursor t direction] is a new state with the cursor fields of [t]
    modified based on [direction]. [t]'s [current_line] field is also updated.
    If we are in the leftmost column and try moving to the left, nothing
    happens. If we are in the top row and try moving up, nothing happens. If we
    are in the bottom row and try moving down, nothing happens. *)

val newline : t -> unit
(** [newline t] inserts a new line to the file corresponding to [t]. *)

val backspace : t -> unit
(** [backspace t] deletes the char to the left of the current cursor pos in the
    file corresponding to [t]. *)

val esc : t -> unit
(*** [esc t] takes [t] out of Insert mode. *)

val write_char : t -> char -> unit
(** [write t c] writes [c] to the file corresponding to [t] at the current
    cursor position. *)

val get_cursor_pos : t -> int * int
(** [get_cursor_pos t] returns the x and y coordinates of [t]'s cursor in a
    tuple. *)

val raw_mode_on : unit -> unit
(** [raw_mode_on ()] puts the terminal into raw mode by adjusting certain flags.
    Raw mode is important because it allows us to detect user keypresses as they
    are inputted, rather than only after a user presses enter, like in a
    terminal's normal mode. *)

val raw_mode_off : unit -> unit
(** [raw_mode_off ()] puts the terminal back into canonical mode. Meant to be
    used after the user has finished editing a file, so the terminal is usable
    like normal again. *)

val set_file : t -> string -> unit
(** [set_file str] sets the terminal to be reading the file with name [str]. *)

val find_indices : string -> string -> int list -> int -> int list
(** [find_indices target str acc start] is the list of the first index of each
    occurrence of [target] in [str], starting from index [start]. *)

val find_and_replace_full : string -> string -> string -> string
(** [find_and_replace_full target str replacement] is the string with all
    occurrences of [target] in [str] with [replacement]. *)
