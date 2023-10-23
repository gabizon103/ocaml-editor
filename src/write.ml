type file = string

let write_file input file =
  let oc = open_out file in
  Printf.fprintf oc "%s" input;
  close_out oc

let make_file str = Sys.command ("touch " ^ str)
