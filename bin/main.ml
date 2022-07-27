open Rescript_linter

let run () =
  let filename = ref "" in
  let process_filename path = filename := path in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " <file>" in
  Arg.parse [] process_filename usage ;
  match String.trim !filename with "" -> Arg.usage [] usage | path -> Linter.run path

let () = run ()
