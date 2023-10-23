open Editor.Command
open Editor.Read
open Editor.Write

let init_config = Editor.Command.create_config ()
let _ = Sys.signal Sys.sigint Sys.Signal_ignore

(* [main ()] starts the text editor. *)

let rec repl () =
  let editor_state = Editor.State.create_state () in
  Editor.State.raw_mode_off ();
  ANSITerminal.print_string [ ANSITerminal.red ] "Please enter command.\n> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> (
      try
        let result = parse str in
        match result with
        | Stats f ->
            (try
               let file_dll = read_file f in
               Editor.State.set_file editor_state f;
               Editor.State.change_dll editor_state file_dll
             with _ -> print_endline "oopsies! file doesn't exist");
            let words = Editor.Stats.get_words editor_state in
            let chars = Editor.Stats.get_chars editor_state in
            let lines = Editor.Stats.get_lines editor_state in
            ANSITerminal.print_string [ Bold; ANSITerminal.red ] "Lines: ";
            ANSITerminal.print_string [ ANSITerminal.white ]
              (string_of_int lines ^ "\n");
            ANSITerminal.print_string [ Bold; ANSITerminal.red ] "Words: ";
            ANSITerminal.print_string [ ANSITerminal.white ]
              (string_of_int words ^ "\n");
            ANSITerminal.print_string [ Bold; ANSITerminal.red ] "Chars: ";
            ANSITerminal.print_string [ ANSITerminal.white ]
              (string_of_int chars ^ "\n")
        | Help ->
            ANSITerminal.print_string
              [ Bold; ANSITerminal.cyan ]
              "\nLED TEXT EDITOR -- MAIN HELP MENU\n";
            ANSITerminal.print_string [ ANSITerminal.red ] " > ";
            ANSITerminal.print_string [ ANSITerminal.white ] "led [filepath]";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\t\tThis command opens the file at the end of [filepath] for \n\
              \                 \t\tediting. You begin in View mode. From \
               there, you can use\n\
              \                 \t\tthe arrow keys to move the cursor around. \
               To enter Insert\n\
              \                 \t\tmode, click the 'i' key on the keyboard. \
               To enter Command\n\
              \                 \t\tmode, hit Ctrl+B. To jump to the start of \
               the file, hit\n\
               \t\t\t\tCtrl+H. To jump to the end of the file, hit Ctrl+T.\n\n";
            ANSITerminal.print_string [ ANSITerminal.red ] " > ";
            ANSITerminal.print_string [ ANSITerminal.white ] "quit";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\t\t\t\tThis command quits the program.\n\n";
            ANSITerminal.print_string [ ANSITerminal.red ] " > ";
            ANSITerminal.print_string [ ANSITerminal.white ] "stats [filepath]";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\t\tThis command displays stats about the file at the end of \n\
               \t\t\t\t[filepath]. Currently displays the number of lines, \
               characters,\n\
               \t\t                and words in the file.\n\n";
            ANSITerminal.print_string [ ANSITerminal.red ] " > ";
            ANSITerminal.print_string [ ANSITerminal.white ]
              "set [attr] [value]";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\t\tThis command is for adjusting your configuration. Available \n\
               \t\t\t\tattributes and their values are:\n\n";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\t\t\t\ttext_color: red, ";
            ANSITerminal.print_string [ ANSITerminal.green ] "green";
            ANSITerminal.print_string [ ANSITerminal.red ] ", ";
            ANSITerminal.print_string [ ANSITerminal.blue ] "blue";
            ANSITerminal.print_string [ ANSITerminal.red ] ", ";
            ANSITerminal.print_string [ ANSITerminal.yellow ] "yellow";
            ANSITerminal.print_string [ ANSITerminal.red ] ", ";
            ANSITerminal.print_string [ ANSITerminal.magenta ] "magenta";
            ANSITerminal.print_string [ ANSITerminal.red ] ", ";
            ANSITerminal.print_string [ ANSITerminal.cyan ] "cyan";
            ANSITerminal.print_string [ ANSITerminal.red ] ", ";
            ANSITerminal.print_string [ ANSITerminal.white ] "white";
            ANSITerminal.print_string
              [ Bold; ANSITerminal.cyan ]
              "\n\nLED TEXT EDITOR -- IN-FILE COMMANDS\n";
            ANSITerminal.print_string [] " :wq";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\t\t\t\tThis command means 'write-quit', which saves the current\n\
              \   \t\t\t\tcontents of the file and quits the program.\n\n";
            ANSITerminal.print_string [] " :fr [target] [replacement]";
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\tThis command is find and replace, which replaces all\n\
              \                 \t\tinstances of [target] with [replacement].";

            ANSITerminal.print_string [] "\n\n";
            repl ()
        | Led f -> (
            try
              let file_dll = read_file f in
              Editor.State.set_file editor_state f;
              Editor.State.change_dll editor_state file_dll;
              Editor.State.raw_mode_off ();
              Editor.State.raw_mode_on ();
              Editor.Draw.draw_loop editor_state init_config
            with _ ->
              (*try begin
                    make_file f;
                    let file_dll = read_file f in
                    Editor.State.set_file editor_state f;
                    Editor.State.change_dll editor_state file_dll;
                    Editor.State.raw_mode_off ();
                    Editor.State.raw_mode_on ();
                    Editor.Draw.draw_loop editor_state init_config end
                with _ -> *)
              repl ())
        | Quit -> Editor.State.raw_mode_off ()
        | Config (attr, v) ->
            (match attr with
            | TextColor -> init_config.textcolor <- v);
            repl ()
        | Rename (oldname, newname) ->
            if Sys.file_exists oldname then (
              Sys.rename oldname newname;
              print_endline "\n")
            else print_endline "file does not exist!\n";
            repl ()
      with
      | Malformed ->
          print_endline "malformed command!\n";
          repl ()
      | Empty ->
          print_endline "empty command\n";
          repl ())

let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1;
  repl ()

let () = main ()
