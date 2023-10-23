(** Reads file contents. *)

open Dlinkedlist

type contents = node
(** [contents] is a type synonym that represents the contents of a file. 
    Each line is stored as an element in the list. *)

type file = string 
(** [file] is a type synoynm representing the name of the file we want to 
    read from. *)

val read_file : file -> contents
(** [read_file str] is the contents of the file with name [str]. *)
