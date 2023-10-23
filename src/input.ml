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

let read_input () =
  let c = Stdlib.input_char stdin in
  match c with
  | '\x1b' -> (
      let keypress1 = Stdlib.input_char stdin in
      let keypress2 = Stdlib.input_char stdin in
      match (keypress1, keypress2) with
      | '[', 'A' -> ArrowUp
      | '[', 'B' -> ArrowDown
      | '[', 'C' -> ArrowRight
      | '[', 'D' -> ArrowLeft
      | _ -> Esc)
  | '^' -> (
      let next1 = Stdlib.input_char stdin in
      let next2 = Stdlib.input_char stdin in
      let next3 = Stdlib.input_char stdin in
      let next4 = Stdlib.input_char stdin in
      let next5 = Stdlib.input_char stdin in
      let next6 = Stdlib.input_char stdin in
      match (next1, next2, next3, next4, next5, next6) with
      | '[', '[', '1', ';', '5', 'C' -> Sel ArrowRight
      | '[', '[', '1', ';', '5', 'D' -> Sel ArrowLeft
      | _ -> Empty)
  | _ -> (
      let c_int = Char.code c in
      match c_int with
      | 9 -> Tab
      | 10 -> Enter
      | 127 -> Backspace
      (* | 31 -> Ctrl '_' *)
      | c_int when 0 <= c_int && c_int <= 31 ->
          Ctrl (Char.chr (Char.code c + 64))
      (* | 113 -> Esc *)
      | _ -> Char c)

(* let read_input () = let c = Stdlib.input_char stdin in if c <> '\x1b' then
   let c_int = Char.code c in match c_int with | 10 -> Enter | 127 -> Backspace
   (* | 31 -> Ctrl '_' *) | c_int when 0 <= c_int && c_int <= 31 -> Ctrl
   (Char.chr (Char.code c + 64)) (* | 113 -> Esc *) | _ -> Char c else let
   keypress1 = Stdlib.input_char stdin in let keypress2 = Stdlib.input_char
   stdin in match (keypress1, keypress2) with | '[', 'A' -> ArrowUp | '[', 'B'
   -> ArrowDown | '[', 'C' -> ArrowRight | '[', 'D' -> ArrowLeft | _ -> Esc *)

(* | (127,_) -> Backspace | (_,_) -> Char keypress1 *)
(** Need to figure out what other inputs look like *)

(** idfk what im doing here *)
