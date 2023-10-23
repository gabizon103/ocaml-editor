(** Parses user commands from terminal *)

type target_file = string
(** The type [parsed_phrase] is a type synonym representing the name of the file
    that the user wants to create, or the name of the existing file that the
    user wants to open.

    If the player command is ["led file.txt"] then [target_file] is
    ["file.txt"]. A player command should only consist of the prefix ["led"]
    followed by a single string. This single string is [target_file]. If the
    user includes leading or trailing spaces in their command, those are not
    included in [target_file]. *)

type attr = TextColor

(** The type [attr] represents an attribute type for the text editor*)


type value = ANSITerminal.style
(** The type [value] is the value for an attribute for the text editor*)

type config_info = { mutable textcolor : value }
(** The type [config_info] represents the config of settings for the editor*)

type command =
  | Led of target_file
  | Help
  | Quit
  | Stats of target_file
  | Config of attr * value
  | Rename of string * string
      (** The type [command] represents a user command that is decomposed into a
          prefix and a file name. Invariant: [target_file] is not empty. *)

exception Malformed
(** Raised when the user input is not formatted properly. This could mean using
    a nonexistent prefix, or including too many arguments for the specified
    prefix. *)

exception Empty
(** Raised when the user does not provide a file target, or does not provide a
    command at all. *)

val create_config : unit -> config_info
(** [create_config ()] creates a new [config_info] record *)

val parse : string -> command
(** [parse str] parses user input into a [command]. The first word (which is a
    consecutive sequence of non-space characters), is the prefix. The second
    word is the target file.

    Requires: [str] includes only alphanumeric characters, as well as
    underscores and hyphens. No spaces.

    Raises: [Malformed] if the prefix is nonexistent, or there are too many
    arguments.

    Raises: [Empty] if there are no arguments provided by the user. *)
