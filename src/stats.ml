(* let get_lines t str = let hd = Dlinkedlist.hd (t.current_line) in

   let get_chars t str = 1 let get_words t str = 1 *)
let get_lines (t : State.t) =
  let hd = Dlinkedlist.hd t.current_line in
  let lst = Dlinkedlist.to_list hd in
  List.length lst

let get_chars (t : State.t) =
  let hd = Dlinkedlist.hd t.current_line in
  let lst = Dlinkedlist.to_list hd in
  List.fold_left (fun acc str -> String.length str + acc) 0 lst

let words_per_line str =
  let lst =
    String.split_on_char ' ' str
    |> List.filter (fun x -> not (String.equal "" x))
  in
  List.length lst

let rec get_words_helper lst acc =
  match lst with
  | [] -> acc
  | h :: t -> get_words_helper t (acc + words_per_line h)

let rec get_words (t : State.t) =
  let hd = Dlinkedlist.hd t.current_line in
  let lst = Dlinkedlist.to_list hd in
  get_words_helper lst 0