open Input
open Unix

(** terminal without any flags changed *)
let origterm =
  let open Unix in
  tcgetattr stdin

(** Represents the mode of the editor. *)
type mode =
  | Insert
  | View
  | Command

type infile_cmd =
  | WQ
  | FR of string * string
  | Unknown
  | Empty

exception Malformed
exception Quit

type t = {
  mutable rows : int option;
  mutable cols : int option;
  mutable cursor_x : int; (* index from 0 *)
  mutable cursor_y : int; (* index from 0 *)
  mutable current_line : Dlinkedlist.node;
  mutable file : string;
  mutable mode : mode;
  mutable clipboard : string;
  mutable selected : string;
  mutable num_lines : int;
  mutable top_line_index : int;
  mutable text_effect : ANSITerminal.style option;
}

let get_num_len i =
  let rec get_num_len_helper acc i =
    if i = 0 then acc else get_num_len_helper (acc + 1) (i / 10)
  in
  get_num_len_helper 0 i

let change_dll t hd =
  t.current_line <- hd;
  t.num_lines <- Dlinkedlist.length hd;
  t.cursor_x <- get_num_len t.num_lines + 2

let change_line t node = t.current_line <- node
let get_cursor_pos t = (t.cursor_x, t.cursor_y)

let create_state () =
  {
    rows = Terminal_size.get_rows ();
    cols = Terminal_size.get_columns ();
    cursor_x = 3;
    cursor_y = 1;
    current_line = Dlinkedlist.empty ();
    file = "";
    mode = View;
    clipboard = "";
    selected = "";
    num_lines = 1;
    top_line_index = 0;
    text_effect = None;
  }

type direction =
  | Up
  | Down
  | Left
  | Right

let term_width, term_height = ANSITerminal.size ()

let raw_mode_on () =
  let term = tcgetattr stdin in
  tcsetattr stdin TCSAFLUSH
    {
      term with
      c_echo = false;
      c_icanon = false;
      c_ignbrk =
        true
        (* c_brkint = false; c_inpck = false; c_istrip = false; c_ixon = false;
           c_icrnl = false; c_opost = false; c_isig = false; c_csize = 8; c_vmin
           = 0; c_vtime = 1; *);
    }

let raw_mode_off () = tcsetattr stdin TCSAFLUSH origterm
let set_file t str = t.file <- str

let check_if_none n =
  match n with
  | Dlinkedlist.None -> false
  | Dlinkedlist.Some r -> true

let change_state_down t =
  let line_num_len = get_num_len t.num_lines + 1 in
  let next_len =
    line_num_len
    + String.length
        (Dlinkedlist.curr_line (Dlinkedlist.get_next t.current_line))
  in
  let move_x = t.cursor_x > next_len in
  t.current_line <- Dlinkedlist.get_next t.current_line;
  t.cursor_y <- t.cursor_y + 1;
  t.cursor_x <- (if move_x then next_len else t.cursor_x)

let change_state_up t =
  let line_num_len = get_num_len t.num_lines + 1 in
  let prev_len =
    line_num_len
    + String.length
        (Dlinkedlist.curr_line (Dlinkedlist.get_prev t.current_line))
  in
  let move_x = t.cursor_x > prev_len in
  t.current_line <- Dlinkedlist.get_prev t.current_line;
  t.cursor_y <- t.cursor_y - 1;
  t.cursor_x <- (if move_x then prev_len else t.cursor_x)

let move_cursor t direction =
  let line_num_len = get_num_len t.num_lines + 1 in
  match direction with
  | Up ->
      if not (Dlinkedlist.is_none (Dlinkedlist.get_prev t.current_line)) then (
        if t.cursor_y > 1 then change_state_up t
        else
          let prev_len =
            line_num_len
            + String.length
                (Dlinkedlist.curr_line (Dlinkedlist.get_prev t.current_line))
          in
          let move_x = t.cursor_x > prev_len in
          t.current_line <- Dlinkedlist.get_prev t.current_line;
          t.top_line_index <- t.top_line_index - 1;
          t.cursor_x <- (if move_x then prev_len else t.cursor_x))
  | Down ->
      if not (Dlinkedlist.is_none (Dlinkedlist.get_next t.current_line)) then (
        if t.cursor_y < term_height - 1 then change_state_down t
        else
          let next_len =
            line_num_len
            + String.length
                (Dlinkedlist.curr_line (Dlinkedlist.get_next t.current_line))
          in
          let move_x = t.cursor_x > next_len in
          t.current_line <- Dlinkedlist.get_next t.current_line;
          t.top_line_index <- t.top_line_index + 1;
          t.cursor_x <- (if move_x then next_len else t.cursor_x))
  | Left -> if t.cursor_x > line_num_len + 1 then t.cursor_x <- t.cursor_x - 1
  | Right ->
      if
        t.cursor_x
        < line_num_len + String.length (Dlinkedlist.curr_line t.current_line)
      then t.cursor_x <- t.cursor_x + 1

(** [change_prev node prev] changes [node] to be pointing to [prev]. *)
let change_prev node prev =
  match node with
  | Dlinkedlist.Some n -> n.back <- prev
  | Dlinkedlist.None -> failwith "None has no backptr"

(** [change_next node next] changes [node] to be pointing to [next]. *)
let change_next node next =
  match node with
  | Dlinkedlist.Some n -> n.next <- next
  | Dlinkedlist.None -> failwith "none has no nextptr"

(** newline has some error that i can't figure out yet *)
let newline t =
  let line_num_len = get_num_len t.num_lines + 1 in
  let s = Dlinkedlist.get_line t.current_line in
  let x_pos = t.cursor_x - line_num_len - 1 in
  let before = String.sub s 0 (if x_pos > 0 then x_pos else 0) in
  let after = String.sub s x_pos (String.length s - x_pos) in
  Dlinkedlist.mutate_val (before ^ "\n") t.current_line;
  Dlinkedlist.insert_after after t.current_line;
  t.current_line <- Dlinkedlist.get_next t.current_line;
  if t.cursor_y < term_height - 1 then t.cursor_y <- t.cursor_y + 1
  else t.top_line_index <- t.top_line_index + 1;
  t.num_lines <- t.num_lines + 1;
  let line_num_len = get_num_len t.num_lines + 1 in
  t.cursor_x <- line_num_len + 1

let remove_char_from_string s x_pos offset =
  let before =
    String.sub s 0 (if x_pos - offset > 0 then x_pos - offset - 1 else 0)
  in
  let after =
    String.sub s (x_pos - offset) (String.length s - x_pos + offset)
  in
  before ^ after

(** TODO: need to account for case where t.cursor_x is 0, so we go to prev line *)
let backspace t =
  let line_num_len = get_num_len t.num_lines + 1 in
  if t.cursor_x > line_num_len + 1 then
    match t.current_line with
    | Dlinkedlist.Some r ->
        r.line <- remove_char_from_string r.line t.cursor_x (line_num_len + 1);
        t.current_line <- Some r;
        t.cursor_x <- (if t.cursor_x = 0 then 0 else t.cursor_x - 1)
    | Dlinkedlist.None -> ()
  else if not (Dlinkedlist.is_none (Dlinkedlist.get_prev t.current_line)) then begin
    t.num_lines <- t.num_lines - 1;
    if t.cursor_y > 1 then t.cursor_y <- t.cursor_y - 1
    else t.top_line_index <- t.top_line_index - 1;
    let line_num_len = get_num_len t.num_lines + 1 in
    t.cursor_x <-
      String.length (Dlinkedlist.get_line (Dlinkedlist.get_prev t.current_line))
      + line_num_len;
    t.current_line <- Dlinkedlist.delete t.current_line
  end

let esc t =
  match t.mode with
  | View -> ()
  | Insert -> t.mode <- View
  | Command -> ()

type node_contents = {
  line : string;
  back : Dlinkedlist.node;
  next : Dlinkedlist.node;
}
(** [node_contents] is a type representing the record type we store in a DLL
    node *)

(** [unbox n] is the record that is in the given node [n]. *)
let unbox (n : Dlinkedlist.node) =
  match n with
  | None -> { line = ""; back = None; next = None }
  | Some { line : string; back : Dlinkedlist.node; next : Dlinkedlist.node } ->
      { line; back; next }

(** [insert_char_to_string c s x_pos] inserts [c] into [s] at the given [x_pos]. *)
let insert_char_to_string (c : char) (s : string) (x_pos : int) (offset : int) =
  if s = "" then String.make 1 c
  else
    let before = String.sub s 0 (x_pos - offset) in
    let after =
      String.sub s (x_pos - offset) (String.length s - x_pos + offset)
    in
    before ^ String.make 1 c ^ after

let write_char t c =
  let line_num_len = get_num_len t.num_lines + 1 in
  match t.current_line with
  | Dlinkedlist.Some r ->
      let new_node =
        Dlinkedlist.Some
          {
            r with
            line = insert_char_to_string c r.line t.cursor_x (line_num_len + 1);
          }
      in
      if r.back <> Dlinkedlist.None then change_next r.back new_node;
      if r.next <> Dlinkedlist.None then change_prev r.next new_node;
      t.current_line <- new_node;
      t.cursor_x <- t.cursor_x + 1
  | Dlinkedlist.None -> ()

let tab t =
  write_char t ' ';
  write_char t ' ';
  write_char t ' ';
  write_char t ' '

let substring_from_to_end str start =
  String.sub str start (String.length str - start)

let substring_from_to_nonincl str start last =
  String.sub str start (last - start)

let rec find_indices target str acc start =
  try
    let i = Str.search_forward (Str.regexp target) str start in
    find_indices target (substring_from_to_end str i) (i :: acc) (i + 1)
  with _ -> acc

let find_first target str start =
  try Str.search_forward (Str.regexp target) str start with _ -> -1

let replace_first target str replacement =
  try
    let i = find_first target str 0 in
    let before = substring_from_to_nonincl str 0 i in
    let after = substring_from_to_end str (i + String.length target) in
    before ^ replacement ^ after
  with _ -> str

let rec find_and_replace_full target str replacement =
  match find_first target str 0 with
  | -1 -> str
  | _ ->
      find_and_replace_full target
        (replace_first target str replacement)
        replacement

(** dll SHOULD BE THE HEAD OF THE DLL!!! *)
let rec find_and_replace_dll target replacement dll =
  try
    let next = Dlinkedlist.get_next dll in
    Dlinkedlist.mutate_val
      (find_and_replace_full target (Dlinkedlist.get_line dll) replacement)
      dll;
    find_and_replace_dll target replacement next
  with _ -> ()

let enter_cmd_mode t =
  t.mode <- Command;
  let rows =
    match t.rows with
    | Some n -> n
    | None -> 0
  in
  t.cursor_x <- 1;
  t.cursor_y <- rows;
  raw_mode_off ()

let parse_infile () =
  let cmd = read_line () in
  match
    String.split_on_char ' ' cmd
    |> List.filter (fun x -> not (String.equal x ""))
  with
  | [] -> Empty
  | h :: t -> begin
      match h with
      | "wq" -> WQ
      | "fr" ->
          if List.length t = 2 then FR (List.nth t 0, List.nth t 1) else Unknown
      | "q" -> raise Quit
      | _ -> Empty
    end

let in_cmd_mode t =
  let cmd = parse_infile () in
  match cmd with
  | WQ ->
      Write.write_file
        (Dlinkedlist.to_string_2 (Dlinkedlist.hd t.current_line))
        t.file;
      raise Quit
  | FR (str1, str2) ->
      find_and_replace_dll str1 str2 (Dlinkedlist.hd t.current_line)
  | Unknown -> ANSITerminal.print_string [] "unknown cmd"
  | Empty -> ()

(* let cmd = read_line () in match cmd with | "wq" -> Write.write_file
   (Dlinkedlist.to_string_2 (Dlinkedlist.hd t.current_line)) t.file | "fr" ->
   find_and_replace_dll "a" "z" (Dlinkedlist.hd t.current_line) | _ -> () *)

let get_next_char t str =
  let line_num_len = get_num_len t.num_lines + 1 in
  String.make 1 str.[t.cursor_x - line_num_len - 1]

let jump_to_head t =
  let line_num_len = get_num_len t.num_lines + 1 in
  t.current_line <- Dlinkedlist.hd t.current_line;
  t.cursor_x <- line_num_len + 1;
  t.cursor_y <- 1;
  t.top_line_index <- 0

let jump_to_tail t =
  let line_num_len = get_num_len t.num_lines + 1 in
  t.current_line <- Dlinkedlist.tl t.current_line;
  t.cursor_x <- line_num_len + 1;
  let size = Dlinkedlist.to_list t.current_line |> List.length in
  if size > term_height - 1 then t.cursor_y <- term_height - 1
  else t.cursor_y <- size;
  t.top_line_index <- t.num_lines - term_height + 1

let process_input t =
  let input = read_input () in
  match t.mode with
  | View -> begin
      match input with
      | ArrowUp -> move_cursor t Up
      | ArrowDown -> move_cursor t Down
      | ArrowLeft -> move_cursor t Left
      | ArrowRight -> move_cursor t Right
      | Esc -> ()
      | Char c ->
          if Char.code c <= 31 && Char.code c >= 0 then enter_cmd_mode t
          else if c = 'i' then t.mode <- Insert
      | Ctrl c -> begin
          match c with
          | 'B' -> enter_cmd_mode t
          | 'C' -> ()
          | 'H' -> jump_to_head t
          | 'T' -> jump_to_tail t
          | _ -> ()
        end
      | Empty -> ()
      | _ -> ()
    end
  | Insert -> begin
      match input with
      | ArrowUp -> move_cursor t Up
      | ArrowDown -> move_cursor t Down
      | ArrowLeft -> move_cursor t Left
      | ArrowRight -> move_cursor t Right
      | Enter -> newline t
      | Sel dir -> (
          match dir with
          | ArrowRight ->
              move_cursor t Right;
              t.selected <-
                t.selected
                ^ (t.current_line |> Dlinkedlist.get_line |> get_next_char t)
          | ArrowLeft -> ()
          | _ -> ())
      | Backspace -> backspace t
      | Esc -> t.mode <- View
      | Tab -> tab t
      (*| Esc -> t.mode <- View*)
      | Char c -> write_char t c
      | Ctrl c -> begin
          match c with
          | 'B' -> enter_cmd_mode t
          | 'C' -> t.clipboard <- t.selected
          | 'H' -> jump_to_head t
          | 'T' -> jump_to_tail t
          | 'U' -> begin
              match t.text_effect with
              | Some v ->
                  if v = ANSITerminal.Underlined then t.text_effect <- None
                  else t.text_effect <- Some ANSITerminal.Underlined
              | None -> t.text_effect <- Some ANSITerminal.Underlined
            end
          | 'D' -> begin
              match t.text_effect with
              | Some v ->
                  if v = ANSITerminal.Bold then t.text_effect <- None
                  else t.text_effect <- Some ANSITerminal.Bold
              | None -> t.text_effect <- Some ANSITerminal.Bold
            end
          | _ -> ()
        end
      | Empty -> ()
    end
  | Command -> in_cmd_mode t
