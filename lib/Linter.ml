let findTextInComments comments needle =
  let f (comment : Res_comment.t) =
    let txt = Res_comment.txt comment in
    String.trim txt = needle
  in
  List.exists f comments

let hasDisableLintComment comments = findTextInComments comments "RSLINT_DISABLE"

let processFile path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel ; src

let lint rules structure comments =
  let errors = ref [] in
  if hasDisableLintComment comments then !errors
  else
    let f acc rule =
      let module R = (val rule : Rule.HASRULE) in
      let name = "RSLINT_DISABLE_" ^ R.meta.ruleName in
      let identifier = "RSLINT_DISABLE_" ^ R.meta.ruleIdentifier in
      if findTextInComments comments name || findTextInComments comments identifier then acc else acc @ [rule]
    in
    let rules = List.fold_left f [] rules in
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
  let ast = Res_core.parse_implementation p in
  match p.diagnostics with
  | [] -> (
      let errors = lint rules ast comments in
      match errors with
      | [] -> print_endline "All good" ; exit 0
      | xs ->
          (let f (msg, loc) = Printer.printError src msg loc in
           List.iter f xs ) ;
          Printer.printHelp () ; exit 1 )
  | diagnostics ->
      (* parser contains problems *)
      Res_diagnostics.print_report diagnostics src ;
      exit 1
