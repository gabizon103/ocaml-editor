(** An interface for drawing output to the terminal. *)

val draw_loop : State.t -> Command.config_info -> unit
(** [draw_loop t c] is the loop that prints out the contents of [t] with the
    attributes stored in [c] to the terminal. *)
