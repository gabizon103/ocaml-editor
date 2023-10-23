type contents = Dlinkedlist.node
type file = string

let read_file str =
  let ic = open_in str in
  let hd = Dlinkedlist.empty () in
  let str_list =
    String.split_on_char '\n' (really_input_string ic (in_channel_length ic))
  in
  let insert_lines n s =
    Dlinkedlist.insert_before (s ^ "\n") (Dlinkedlist.hd n)
  in
  let rev_str_list = List.rev str_list in
  Dlinkedlist.insert_after (List.hd rev_str_list) hd;
  List.iter (insert_lines hd) (List.tl rev_str_list);
  Dlinkedlist.hd hd
