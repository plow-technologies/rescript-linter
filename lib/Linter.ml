open Rescript_parser

let hasDisableLintComment comments =
  let f (comment : Res_comment.t) =
    let txt = Res_comment.txt comment in
    String.trim txt = "RSLINT_DISABLE"
  in
  List.exists f comments

let processFile path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel ; src

let lint rules structure comments =
  let errors = ref [] in
  if hasDisableLintComment comments then !errors
  else
    let callback (pair : string * Location.t) = errors := !errors @ [pair] in
    let iterator = Iterator.makeIterator rules callback in
    iterator.structure iterator structure ;
    !errors

let run configPath path =
  let rules = ConfigReader.parseConfig configPath in
  let src = processFile path in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  let p = Res_parser.make ~mode:Res_parser.Default src path in
  let comments = p.comments in
  let ast = Res_core.parseImplementation p in
  match p.diagnostics with
  | [] -> (
      let errors = lint rules ast comments in
      match errors with
      | [] -> print_endline "All good" ; exit 0
      | xs ->
          (let f (msg, loc) = Printer.printError src msg loc in
           List.iter f xs ) ;
          exit 1 )
  | diagnostics ->
      (* parser contains problems *)
      Res_diagnostics.printReport diagnostics src ;
      exit 1
