open Rescript_linter

let run () =
  let filename = ref "" in
  let configfile = ref "" in
  let process_filename path = filename := path in
  let usage = "Usage: " ^ Sys.argv.(0) ^ "-c <config file path> <file to lint>" in
  let speclist = [("-c", Arg.Set_string configfile, "Set config file path")] in
  Arg.parse speclist process_filename usage ;
  match String.trim !filename with
  | "" -> Arg.usage speclist usage
  | path -> (
    match String.trim !configfile with
    | "" -> Arg.usage speclist usage
    | configfile -> Linter.run configfile path )

let () = run ()
