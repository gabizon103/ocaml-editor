(** Doubly Linked List of strings to represent lines of a file*)

(** [node] represents a doubly linkedlist that contains strings*)
type node = 
| None
| Some of 
{
    mutable line: string; 
    mutable back: node; 
    mutable next: node
}

exception EmptyList
(** [EmptyList] is an exception for when list is empty*)

val empty : unit -> node 
(** [empty ()] returns an empty node*)

val is_empty : node -> bool
(** [is_empty node] is true if the node is empty*)

val get_line : node -> string
(** [get_line n] is the string inside of [n]*)

val get_next : node -> node
(** [get_next node] is the node that comes after [node]*)

val get_prev : node -> node
(** [get_prev node] is the node that comes before [node]*)

val is_none : node -> bool
(** [is_none node] is true if the node is none*)

val length : node -> int
(** [length node] is the number of nodes in [node]*)

val mutate_val : string -> node -> unit
(** [mutate_val s node] changes the string inside of [node] to [s]*)

val insert_after : string -> node -> unit
(** [insert_after n1 n2] inserts [n1] after [n2]*)

val insert_before : string -> node -> unit
(** [insert_before n1 n2] inserts [n1] before [n2]*)

val delete : node -> node
(** [delete n] removes [n] from the linked list*)

val hd : node -> node
(** [hd n] is the head of the linked list that contains [n]*)

val tl : node -> node
(** [tl n] is the tail of the linked list that contains [n]*)

val to_list : node -> string list
(** [to_list n] is a list representation of the entire linked list*)

val to_string : node -> string
(** A string representation of the entire linked list*)

val to_string_2 : node -> string
(** A string representation of the entire linked list without formatting*)

val curr_line : node -> string
(** Same as get_line, gets the contents of the node*)
