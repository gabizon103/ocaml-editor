(**A [dlinkedlist]*)

type node =
  | None
  | Some of {
      mutable line : string;
      mutable back : node;
      mutable next : node;
    }

let empty () = Some { line = ""; back = None; next = None }

exception EmptyList

let is_empty = function
  | None -> false
  | Some n -> n.line = "" && n.back = None && n.next = None

let get_line = function
  | None -> raise EmptyList
  | Some lst -> lst.line

let get_next = function
  | None -> raise EmptyList
  | Some lst -> lst.next

let get_prev = function
  | None -> raise EmptyList
  | Some lst -> lst.back

let is_none = function
  | None -> true
  | _ -> false

let length = function
  | None -> failwith "Can't get length of None (This should never happen)"
  | l ->
      if is_empty l then 0 else
      let rec length_helper acc lst =
        let nxt = get_next lst in
        if is_none nxt then acc else length_helper (acc + 1) nxt
      in
      length_helper 1 l

let mutate_val n = function
  | None -> raise EmptyList
  | Some node ->
      (* if is_empty (Some node) then raise EmptyList else  *)
      (* why can't we mutate empty node? no way else to make lists*)
      node.line <- n

let insert_after n = function
  | None -> failwith "Can't insert from None (This should never happen)"
  | Some node -> (
      if is_empty (Some node) then node.line <- n
      else
        let old_next = node.next in
        (* a new line *)
        let new_node = Some { line = n; back = Some node; next = old_next } in
        node.next <- new_node;
        match old_next with
        | None -> ()
        | Some old -> old.back <- new_node)

let insert_before n = function
  | None -> failwith "Can't insert from None (This should never happen)"
  | Some node -> (
      if is_empty (Some node) then node.line <- n
      else
        let old_back = node.back in
        let new_node = Some { line = n; back = old_back; next = Some node } in
        node.back <- new_node;
        match old_back with
        | None -> ()
        | Some old -> old.next <- new_node)

let clean_str s = String.sub s 0 (String.length s - 1)

let delete = function
  | None -> raise EmptyList
  | Some node -> (
      match (node.back, node.next) with
      | None, None -> Some node
      | Some b, None ->
          b.next <- None;
          b.line <- clean_str b.line ^ node.line;
          Some b
      | None, Some n -> Some node
      | Some b, Some n ->
          b.next <- Some n;
          n.back <- Some b;
          b.line <- clean_str b.line ^ node.line;
          Some b)

let rec hd = function
  | None -> failwith "Can't have head of None (This should never happen)"
  | Some node -> if node.back = None then Some node else hd node.back

let rec tl = function
  | None -> failwith "Can't have tail of None (This should never happen)"
  | Some node -> if node.next = None then Some node else tl node.next

let rec to_list_helper acc = function
  | None -> List.rev acc
  | Some node -> to_list_helper (node.line :: acc) node.next

let to_list = function
  | None -> failwith "Can't have list of None (This should never happen)"
  | Some node -> to_list_helper [] (hd (Some node))

let to_string lst =
  let nlst = to_list lst in
  List.fold_left (fun acc s -> acc ^ s ^ ", ") "[" nlst ^ "]"

let to_string_2 lst =
  let nlst = to_list lst in
  (List.fold_left (fun acc s -> acc ^ s)) "" nlst

let curr_line = function
  | None -> failwith "Current Line is Empty"
  | Some node -> node.line
