(** Draws to the terminal *)

open State

(** [move_cursor_display t] moves the cursor based on state [t] cursor pos*)
let move_cursor_display (t : State.t) =
  ANSITerminal.set_cursor t.cursor_x t.cursor_y

(** [add_line_nums content num] adds incrementing line numbers [num] spaced out
    accordingly based on [lines_len] to each string in list [content]*)
let rec add_line_nums content num lines_len =
  match content with
  | [] -> []
  | h :: t ->
      let str_num = string_of_int num in
      (str_num ^ String.make (lines_len - String.length str_num + 1) ' ' ^ h)
      :: add_line_nums t (num + 1) lines_len

(** [substring_from_to_end str start] gets the substring of [str] from position
    [start] to the end of the [str]*)
let substring_from_to_end str start =
  String.sub str start (String.length str - start)

(** [substring_from_to_nonincl str start last] gets the substring of [str] from
    position [start] to position [last] *)
let substring_from_to_nonincl str start last =
  String.sub str start (last - start)

(** [find_str_wrapped str target] finds the position of the first character of
    [str] in [target]. If not found, return -1*)
let find_str_wrapped str target =
  try Str.search_forward (Str.regexp target) str 0 with _ -> -1

(** [find_key_word_match] finds the position of the first occurance of "match"
    in a line if the file opened is a .ml file. If not a .ml file, return \-1*)
let find_keyword_match t str =
  if String.equal (String.sub str (String.length str - 3) 3) ".ml" then
    find_str_wrapped str "match"
  else -1

(** [color_first_match color str] colors the first occurance of "match" in a .ml
    file. [color] is the original text color.*)
let color_first_match textcolor str =
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Bold ]
    (String.sub str 0 2);
  let i = find_str_wrapped str "match" in
  if i >= 0 then begin
    let before = substring_from_to_nonincl str 0 i in
    let after = substring_from_to_end str (i + String.length "match") in

    ANSITerminal.print_string [ textcolor ]
      (String.sub before 2 (String.length before - 2));
    ANSITerminal.print_string [ ANSITerminal.green ] (String.sub str i 5);
    ANSITerminal.print_string [ textcolor ] after
  end
  else
    ANSITerminal.print_string [ ANSITerminal.white ]
      (String.sub str 2 (String.length str - 2))

(** [print_line line] prints a string [line] making the line number blue and
    bold*)
let print_line textcolor (t : State.t) line =
  let text_effect =
    match t.text_effect with
    | Some v -> [ v ]
    | None -> []
  in
  let rec find_line_start s acc whitespace =
    if (not whitespace) && String.get s 0 <> ' ' then
      find_line_start (String.sub s 1 (String.length s - 1)) (acc + 1) false
    else if String.get s 0 = ' ' then
      find_line_start (String.sub s 1 (String.length s - 1)) (acc + 1) true
    else acc
  in
  let start_pos = find_line_start line 0 false in
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Bold ]
    (String.sub line 0 start_pos);
  ANSITerminal.print_string
    (List.flatten [ [ textcolor ]; text_effect ])
    (String.sub line start_pos (String.length line - start_pos))

(** returns index of first selected char *)
(* let find_highlighted (t : State.t) str = let highlighted = t.selected in
   Str.search_forward (Str.regexp highlighted) (t.current_line |>
   Dlinkedlist.get_line) 0 *)

(* let print_line_highlights (t : State.t) line = let first_highlighted_index =
   find_highlighted t line in let before = String.sub line 0
   first_highlighted_index in let after = String.sub line
   first_highlighted_index (first_highlighted_index + String.length t.selected)
   in ANSITerminal.print_string [ ANSITerminal.blue; ANSITerminal.Bold ]
   (String.sub line 0 2); ANSITerminal.print_string [ ANSITerminal.white ]
   before; ANSITerminal.print_string [ ANSITerminal.on_cyan; ANSITerminal.white
   ] t.selected; ANSITerminal.print_string [ ANSITerminal.white ] after *)

(** [print_lines content height] prints out each string of list [content] as a
    line limited to [height]*)
let print_lines content height (conf : Command.config_info) start (t : State.t)
    =
  let content_lines =
    add_line_nums content 1
      (String.length (string_of_int (List.length content)))
  in
  let rec sub start len lst =
    if start > 0 then
      match lst with
      | [] -> []
      | _ :: t -> sub (start - 1) len t
    else if len = 0 then []
    else
      match lst with
      | [] -> []
      | h :: t -> h :: sub 0 (len - 1) t
  in
  let content_sub = sub start (height - 1) content_lines in
  let _ = List.map (print_line conf.textcolor t) content_sub in
  ()

(** [reset_screen ()] clears the screen and resets the position of the cursor *)
let reset_screen () =
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1

(** [edit_file content] allows the user to edit a file represented as a list of
    strings in [content]*)
let edit_file content conf start (t : State.t) =
  reset_screen ();
  let width, height = ANSITerminal.size () in
  print_lines content height conf start t

(** = a string representation of the editing mode that [state] is in*)
let to_string_mode t =
  match t.mode with
  | Insert -> "insert"
  | View -> "view"
  | Command -> "cmd"

(** [get_num_len i] returns the number of digits in int [i]*)
let get_num_len i =
  let rec get_num_len_helper acc i =
    if i = 0 then acc else get_num_len_helper (acc + 1) (i / 10)
  in
  get_num_len_helper 0 i

(** [draw_loop state config] continually draws the terminal based on [state] and
    [config] until the program is quit*)
let draw_loop (t : State.t) (conf : Command.config_info) =
  let line_num_len = get_num_len t.num_lines + 1 in
  edit_file
    (Dlinkedlist.to_list (Read.read_file t.file))
    conf t.top_line_index t;
  ANSITerminal.set_cursor (line_num_len + 1) 1;
  while true do
    State.process_input t;
    edit_file (Dlinkedlist.to_list t.current_line) conf t.top_line_index t;
    move_cursor_display t (* print_endline (to_string_mode t) *)
  done
