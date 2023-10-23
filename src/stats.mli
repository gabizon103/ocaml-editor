(** An interface used for computing stats about a file *)

val get_lines : State.t -> int
(** [get_lines t] is the number of lines in the file that [t] currently holds,
    as indicated by the length of the DLL stored in it. *)

val get_chars : State.t -> int
(** [get_chars t] is the number of characters in the file that [t] currently
    holds. *)

val get_words : State.t -> int
(** [get_words] is the number of words in the file [t] currently holds, where
    each word is a block of chars that is separated from the next by at least
    one space. *)
