type target_file = string

type command =
  | Led of target_file
  | Help
  | Quit
  | Stats of target_file
  | Config of attr * value
  | Rename of string * string

and attr = TextColor
and value = ANSITerminal.style

type config_info = { mutable textcolor : value }

let create_config () = { textcolor = ANSITerminal.white }

exception Empty
exception Malformed

(** [fst lst] is the first element of lst, or the empty string if lst is empty. *)
let fst = function
  | [] -> ""
  | h :: t -> h

let parse str =
  if String.equal str "" then raise Empty
  else
    match
      String.split_on_char ' ' str
      |> List.filter (fun x -> not (String.equal x ""))
    with
    | [] -> raise Empty
    | h :: t -> (
        let target = List.filter (fun x -> not (String.equal x "go")) t in
        match h with
        | "led" ->
            if List.length target = 1 then Led (target |> fst)
            else if List.length target > 1 then raise Malformed
            else raise Empty
        | "help" -> Help
        | "quit" -> Quit
        | "rename" ->
            if List.length target = 2 then Rename (List.nth t 0, List.nth t 1)
            else raise Malformed
        | "stats" ->
            if List.length target = 1 then Stats (List.nth target 0)
            else raise Malformed
        | "set" ->
            if List.length target = 2 then
              match List.nth target 0 with
              | "text_color" ->
                  let v =
                    match List.nth target 1 with
                    | "red" -> ANSITerminal.red
                    | "green" -> ANSITerminal.green
                    | "blue" -> ANSITerminal.blue
                    | "yellow" -> ANSITerminal.yellow
                    | "magenta" -> ANSITerminal.magenta
                    | "cyan" -> ANSITerminal.cyan
                    | "white" -> ANSITerminal.white
                    | _ -> raise Malformed
                  in
                  Config (TextColor, v)
              | _ -> raise Malformed
            else raise Malformed
        | _ -> raise Empty)

(* if (String.equal h "led" && List.length target = 1) then Led (target |> fst)
   else if (String.equal h "help" && List.length target = 0) then Help else if
   (not (String.equal h "led")) then raise Malformed else if (String.equal h
   "led" && List.length target > 1) then raise Malformed else raise Empty *)
