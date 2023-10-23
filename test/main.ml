open OUnit2
open Editor
open Command
open Read
open State
open Input
open Write
open Dlinkedlist

(******************************************************************************
  _______                _       _____    _
  |__   __|             | |     |  __ \  | |
    | |      ___   ___  | |_    | |__) | | |   __ _   _ __
    | |     / _ \ / __| | __|   |  ___/  | |  / _` | | '_ \
    | |    |  __/ \__ \ | |_    | |      | | | (_| | | | | |
    |_|     \___| |___/  \__|   |_|      |_|  \__,_| |_| |_|

    In order to effectively develop and test our project, we decided to use
    test-driven development with OUnit testing wherever possible and manually
    test parts that were difficult or not possible to use OUnit tests with.
    As a lot of our project depends on the user experience, there was a lot
    that could not be tested using OUnit tests, though lots of core functions
    were tested and developed using test-driven development.

    Most of the OUnit tests for our project was for the backend
    functions, like reading, writing, data structures, and parsing. Manual
    testing was done for testing scrolling, drawing, shortcuts, behavior of
    commands, some inputs, and more holisitic behaviors of the program.
    In addition, as a lot of our project uses mutability, OUnit testing and TDD
    was mainly used to test the basic functionality of many functions and some
    edge cases. More extensive testing of mutable functions was done manually.
    When writing test functions when dealing with mutable aspects of the
    project, care was taken to ensure that a new "object" was created for
    every test for consistency.

    Test cases were developed using a mixture of
    glass and black box testing. Test cases would first be written with
    black box testing and testing basic usage of the function, and as the
    function was developed the developer would add glass box tests to ensure
    that edge cases would pass.

    Our test suite demonstrates the correctness of the system as it tests the
    core functionality of our project. Reading files, writing to files, parsing
    commands, our custom doubly linked list implementation, and some user inputs
    were all tested and developed using the test suite. These are all crucial to
    our working text editor, so care was taken to make sure that these functions
    worked before other functions were developed that relied on them.

    ***************************************************************************)

(***********************************************************************
                                COMMAND TESTS
  ************************************************************************)

(** [parse_test name input expected_output] compares [parse input] against
    [expected_output]. *)
let parse_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (parse input)

(** [parse_test_invalid name input expected_output] compares [parse input]
    against [expected_output]. *)
let parse_test_invalid name input expected_output =
  name >:: fun _ -> assert_raises expected_output (fun () -> parse input)

(** [get_target cmd] is the target file of a given [command]. *)
let get_target = function
  | Led target -> target
  | _ -> ""

let command_tests =
  [
    parse_test "parse \"led file.txt\"" "led file.txt" (parse "led file.txt");
    parse_test_invalid "parse \"bruh file.txt\"" "bruh file.txt" Empty;
    parse_test_invalid "parse \"\"" "led " Empty;
    parse_test_invalid "parse \"\"" "" Empty;
    parse_test_invalid "parse \" \"" " " Empty;
  ]

(***********************************************************************
                                READ TESTS
  ************************************************************************)

(** [read_file_test name input expected_output] compares [read_file input]
    against [expected_output]. *)
let read_file_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (to_list (read_file input))
    ~printer:(String.concat ",")

let data_dir_prefix = "data" ^ Filename.dir_sep
let read_tests_prefix = data_dir_prefix ^ "read_tests" ^ Filename.dir_sep

let read_tests =
  [
    read_file_test "read testfile"
      (read_tests_prefix ^ "testfile")
      [ "helloooo\n"; "team led\n"; "bruh\n"; "bruh1\n"; "bruh 2\n" ];
    read_file_test "read many lines"
      (read_tests_prefix ^ "many_lines")
      [ "\n"; "\n"; "\n"; "\n" ];
    read_file_test "read newlines"
      (read_tests_prefix ^ "newlines")
      [ "hello\\n\n"; "\\n this is a test\n" ];
    read_file_test "read empty" (read_tests_prefix ^ "empty") [ "" ];
  ]

(***********************************************************************
                                  WRITE TESTS
  ************************************************************************)

let int_to_unit = function
  | _ -> ()

let write_tests_prefix =
  data_dir_prefix ^ Filename.dir_sep ^ "write_tests" ^ Filename.dir_sep

let write_file_test name expected input =
  name >:: fun _ ->
  Sys.command ("touch " ^ write_tests_prefix ^ name) |> int_to_unit;
  Write.write_file input (write_tests_prefix ^ name);
  assert_equal expected
    (Dlinkedlist.to_string_2 (Read.read_file (write_tests_prefix ^ name)))
    ~printer:Fun.id

let write_tests =
  [
    write_file_test "write_testfile" "helloooo\nteam led\nbruh\nbruh1\nbruh 2\n"
      "helloooo\nteam led\nbruh\nbruh1\nbruh 2\n";
    write_file_test "write_empty" "" "";
    write_file_test "write_lines" "\n\n\n" "\n\n\n";
    write_file_test "write_many_whitespace" "HEADER\n\nsubtext\n"
      "HEADER\n\nsubtext\n";
    write_file_test "write_hellotest" "hello\\n\n\\nthis is a test\n"
      "hello\\n\n\\nthis is a test\n";
  ]

(***********************************************************************
                                STATE TESTS
  ************************************************************************)

(** [print_coords (x,y)] is a printer for checking correct cursor position. *)
let print_coords = function
  | x, y -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let s1 = create_state () (* 3,1 *)
let s2 = create_state () (* 3,1 *)
let dll_movetest1 = Dlinkedlist.empty ()

let _ =
  Dlinkedlist.mutate_val "bb" dll_movetest1;
  Dlinkedlist.insert_after "bb" dll_movetest1;
  Dlinkedlist.insert_after "bb" (Dlinkedlist.get_next dll_movetest1);
  s2.current_line <- dll_movetest1

let _ =
  move_cursor s2 Down;
  move_cursor s2 Down;
  (* y = 3*)
  move_cursor s2 Right (* x = 3 *)

let s3 = create_state () (* 3,1 *)
let dll_movetest2 = Dlinkedlist.empty ()

let _ =
  Dlinkedlist.mutate_val "bb" dll_movetest2;
  Dlinkedlist.insert_after "bb" dll_movetest2;
  Dlinkedlist.insert_after "bb" (Dlinkedlist.get_next dll_movetest2);
  s3.current_line <- dll_movetest2

let _ =
  move_cursor s3 Down;
  move_cursor s3 Down;
  move_cursor s3 Down

let s4 = create_state ()

let _ =
  newline s4;
  newline s4

let move_cursor_test name state dir expected_output =
  name >:: fun _ ->
  move_cursor state dir;
  assert_equal expected_output (get_cursor_pos state) ~printer:print_coords

let get_str (node : Dlinkedlist.node) : string =
  match node with
  | Dlinkedlist.Some r -> r.line
  | Dlinkedlist.None -> failwith "no string here"

let get_back n =
  match n with
  | Dlinkedlist.Some node -> node.back
  | Dlinkedlist.None -> failwith "None has no backptr"

let get_next n =
  match n with
  | Dlinkedlist.Some node -> node.next
  | Dlinkedlist.None -> failwith "None has no nextptr"

let n1 = Dlinkedlist.empty ()
let s5 = create_state () (* cursor_pos 0 0*)
let _ = Dlinkedlist.mutate_val "s1" n1
let _ = Dlinkedlist.insert_after "s2" n1

let _ =
  Dlinkedlist.insert_after "s3" (get_next n1);
  s5.current_line <- get_next n1
(* now have a linked list n1 -> n2 -> n3 *)

let s6 = create_state ()
let _ = s6.current_line <- get_next n1 |> get_next
let s7 = create_state ()
let _ = s7.current_line <- n1
let s8 = create_state ()
let s8_string = Dlinkedlist.empty ()

let _ =
  Dlinkedlist.mutate_val "hello " s8_string;
  s8.current_line <- s8_string

let dll1n1 = Dlinkedlist.empty ()
let dll1s1 = create_state ()
let _ = dll1s1.current_line <- dll1n1

let _ =
  Dlinkedlist.mutate_val "hello" dll1n1;
  (* now dll1n1 has 'hello' in it*)
  Dlinkedlist.insert_after "im" dll1n1;
  Dlinkedlist.insert_after "ethan" (Dlinkedlist.get_next dll1n1)
(* now dll1 has 'hello' -> 'im' -> 'ethan' *)

let dll2n1 = Dlinkedlist.empty ()
let dll2s1 = create_state ()

let _ =
  Dlinkedlist.mutate_val "yooo" dll2n1;
  (* now dll2n1 has "yooo" in it *)
  Dlinkedlist.insert_after "i love 3110" dll2n1;
  Dlinkedlist.insert_after "haha" (Dlinkedlist.get_next dll2n1);
  dll2s1.current_line <- Dlinkedlist.get_next dll2n1
(* points to n2 *)
(* now dll2 has "yooo" -> "i love 3110" -> "haha" *)

let r1 = Str.regexp "bruh"

let write_char_test name state c x_pos mid expected_str expected_cursor =
  name >:: fun _ ->
  state.cursor_x <- x_pos;
  write_char state c;
  assert_equal expected_str (get_str state.current_line);
  assert_equal expected_cursor state.cursor_x;
  if mid then (
    assert_equal expected_str
      (get_back state.current_line |> get_next |> get_str);
    assert_equal expected_str
      (get_next state.current_line |> get_back |> get_str))

let backspace_test name x_pos mid input expected_str expected_cursor =
  name >:: fun _ ->
  let t_state = create_state () in
  let t_node = Dlinkedlist.empty () in
  Dlinkedlist.mutate_val "line before \n" t_node;
  Dlinkedlist.insert_after input t_node;
  t_state.current_line <- Dlinkedlist.get_next t_node;
  t_state.cursor_x <- x_pos + 3;
  backspace t_state;
  assert_equal ~printer:(fun s -> s) expected_str (get_str t_state.current_line);
  assert_equal
    ~printer:(fun s -> string_of_int s)
    (expected_cursor + 3) t_state.cursor_x;
  if mid then (
    assert_equal
      ~printer:(fun s -> s)
      expected_str
      (get_back t_state.current_line |> get_next |> get_str);
    assert_equal
      ~printer:(fun s -> s)
      expected_str
      (get_next t_state.current_line |> get_back |> get_str))

let find_indices_test name target str acc start expected =
  name >:: fun _ ->
  assert_equal
    ~printer:(fun lst ->
      "[" ^ List.fold_left (fun acc i -> string_of_int i ^ acc) "" lst ^ "]")
    expected
    (State.find_indices target str acc start)

let find_and_replace_full_test name target str replacement expected =
  name >:: fun _ ->
  assert_equal expected (State.find_and_replace_full target str replacement)

let state_tests =
  [
    move_cursor_test "move cursor down in empty state" s1 Down (3, 1);
    move_cursor_test "move cursor left in empty" s1 Left (3, 1);
    move_cursor_test "move cursor right in empty" s1 Right (3, 1);
    move_cursor_test "move cursor up in empty" s1 Up (3, 1);
    move_cursor_test "move s2 right" s2 Right (4, 3);
    move_cursor_test "move s3 down" s3 Down (3, 3);
    write_char_test "write 7 to dll1n1" dll1s1 '7' 4 false "h7ello" 5;
    write_char_test "write 9 to i dll2n2" dll2s1 '9' 6 true "i l9ove 3110" 7;
    backspace_test "backspace h from hello" 1 false "hello " "ello " 0;
    backspace_test "backspace o from hello" 5 false "hello " "hell " 4;
    backspace_test "beginning of line backspace" 0 false "hello "
      "line before hello " 11;
    backspace_test "backspace empty line" 0 false "" "line before " 11;
    backspace_test "backspace bruh" 9 false "bruh bruh bruh" "bruh bru bruh" 8;
    backspace_test "backspace bruh beginning" 0 false "bruh bruh bruh"
      "line before bruh bruh bruh" 11;
    find_indices_test "find bruh in r1" "bruh" "bruhfuckbruhfuck" [] 0 [ 8; 0 ];
    find_and_replace_full_test "replace a with c in abab" "a" "abab" "c" "cbcb";
    find_and_replace_full_test "replace ab with c" "ab" "abab" "c" "cc";
  ]

(***********************************************************************
                            DLINKEDLIST TESTS
  ************************************************************************)
let test_empty_exception name f line =
  name >:: fun _ -> assert_raises Dlinkedlist.EmptyList (fun () -> f line None)

let test_empty_exception1 name f =
  name >:: fun _ -> assert_raises Dlinkedlist.EmptyList (fun () -> f None)

let empty_lst = empty ()
let basic = empty ()
let long = empty ()

let _ =
  insert_after "test" basic;
  insert_after "this" long;
  insert_after "long" long;
  insert_after "test" long;
  insert_after "is" long;
  insert_after "cool" long

let test_is_empty name dll expected_output =
  name >:: fun _ -> assert_equal (is_empty dll) expected_output

let test_to_list name dll expected_output =
  name >:: fun _ -> assert_equal (to_list dll) expected_output

let test_mutate_val name dll nval expected_output =
  name >:: fun _ ->
  let node = empty () in
  mutate_val nval node;
  match node with
  | Some n -> assert_equal expected_output n.line
  | None -> failwith "Not Possible"

let test_insert_after name dll next expected_output =
  name >:: fun _ ->
  let node = empty () in
  insert_after "test" node;
  insert_after next node;
  assert_equal ~printer:(String.concat ",") expected_output (to_list node)

let test_insert_before name dll back expected_output =
  name >:: fun _ ->
  let node = empty () in
  insert_after "test" node;
  insert_before back node;
  assert_equal ~printer:(String.concat ",") expected_output (to_list node)

let test_hd name dll expected_output =
  name >:: fun _ ->
  match hd dll with
  | None -> failwith "Not Possible"
  | Some lst -> assert_equal lst.line expected_output

let test_tl name dll expected_output =
  name >:: fun _ ->
  match tl dll with
  | None -> failwith "Not Possible"
  | Some lst -> assert_equal lst.line expected_output

let test_length name dll expected_output =
  name >:: fun _ -> assert_equal (length dll) expected_output

let test_get_next name dll expected_output =
  name >:: fun _ -> assert_equal (get_next dll) expected_output

let dll_tests =
  [
    test_empty_exception "Mutate Empty" mutate_val "Test";
    test_empty_exception1 "Get Next" Dlinkedlist.get_next;
    test_empty_exception1 "Get Prev" get_prev;
    test_length "empty length" empty_lst 0;
    test_is_empty "empty true" empty_lst true;
    test_is_empty "basic false" basic false;
    test_to_list "basic list" basic [ "test" ];
    test_mutate_val "basic test" basic "new test" "new test";
    test_insert_after "basic after" basic "after" [ "test"; "after" ];
    test_insert_before "basic before" basic "before" [ "before"; "test" ];
    test_length "basic length" basic 1;
    test_to_list "long list" long [ "this"; "cool"; "is"; "test"; "long" ];
    test_hd "long hd" long "this";
    test_hd "long hd nxt" (get_next long) "this";
    test_hd "long hd next 2" (get_next (get_next long)) "this";
    test_tl "long tl" long "long";
    test_tl "long tl nxt" (get_next long) "long";
    test_tl "long tl next 2" (get_next (get_next long)) "long";
    test_length "long length" long 5;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [ command_tests; read_tests; write_tests; state_tests; dll_tests ]

let _ = run_test_tt_main suite
