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
  (* Track errors and warnings separately *)
  let errors = ref [] in
  let warnings = ref [] in
  (* If there's a disable comment, skip linting, including warnings *)
  if hasDisableLintComment comments then (!errors, !warnings)
  else
    let f acc rule =
      let module R = (val rule : Rule.HASRULE) in
      let name = "RSLINT_DISABLE_" ^ R.meta.ruleName in
      let identifier = "RSLINT_DISABLE_" ^ R.meta.ruleIdentifier in
      if findTextInComments comments name || findTextInComments comments identifier then acc else acc @ [rule]
    in
    let rules = List.fold_left f [] rules in
    (* Create the respecitive callbacks for tracking errors and warnings *)
    let errorCallback (pair : string * Location.t) = errors := !errors @ [pair] in
    let warningCallback (pair : string * Location.t) = warnings := !warnings @ [pair] in
    (* Run the linter *)
    let iterator = Iterator.makeIterator rules {errorCallback; warningCallback} in
    iterator.structure iterator structure ;
    (* Report both errors and warnings *)
    (!errors, !warnings)

type lintInfo =
  { message: string
  ; location: Location.t
        [@to_yojson Printer.JSON.yojson_of_location] [@of_yojson Printer.JSON.location_of_yojson]
  ; pretty: Printer.PrettyPrint.output }
[@@deriving yojson]

let lintInfo_of_tuple_with_message_kind (message_kind : Printer.PrettyPrint.message_kind) src (msg, loc) =
  {message= msg; location= loc; pretty= Printer.PrettyPrint.asOutput message_kind src msg loc}

type jsonOutput = {errors: lintInfo list; warnings: lintInfo list} [@@deriving yojson]

let run configPath path (outputJson : bool) =
  let rules =
    try ConfigReader.parseConfig configPath
    with ConfigReader.ConfigParseError msg ->
      Printf.eprintf "Config Error: %s\n" msg ;
      exit 1
  in
  if not outputJson then
    Format.fprintf Format.std_formatter "Linting rules:\n%s"
      (String.concat "\n"
         (List.map
            (fun rule ->
              let module R = (val rule : Rule.HASRULE) in
              Rule.meta_to_string R.meta )
            rules ) ) ;
  let src = processFile path in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  let p = Res_parser.make ~mode:Res_parser.Default src path in
  let comments = p.comments in
  let ast = Res_core.parse_implementation p in
  match p.diagnostics with
  | [] -> (
      let errors, warnings = lint rules ast comments in
      match outputJson with
      | true ->
          Yojson.Safe.pretty_print Format.std_formatter
            (jsonOutput_to_yojson
               { errors= List.map (lintInfo_of_tuple_with_message_kind `error src) errors
               ; warnings= List.map (lintInfo_of_tuple_with_message_kind `warning src) warnings } )
      | false -> (
          (* Always print warnings *)
          List.iter (fun (msg, loc) -> Printer.PrettyPrint.warning src msg loc) warnings ;
          (* Print an extra newline if there were warnings *)
          if List.length warnings > 0 then print_newline () ;
          match errors with
          | [] -> print_endline "All good" ; exit 0
          | xs ->
              (let f (msg, loc) = Printer.PrettyPrint.error src msg loc in
               List.iter f xs ) ;
              Printer.printHelp () ; exit 1 ) )
  | diagnostics ->
      (* parser contains problems *)
      Res_diagnostics.print_report diagnostics src ;
      exit 1
