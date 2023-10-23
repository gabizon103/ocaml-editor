(** Interface that handles writing to files. *)

type file = string
(** [file] is a type synonym that represents the file name we want to edit. *)

val write_file : file -> string -> unit
(** [write input file] writes [input] to [file] *)

val make_file : file -> int
(** [make_file str] creates a file with name [str]. *)

(* val edit_file : file -> char -> int * int -> unit  *)
(* [edit_file file] *)
