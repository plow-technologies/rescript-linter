open Rescript_parser

let processFile path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel;
  src

let filename = "./test/foo.res"

let src = processFile filename

let p =
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  Res_parser.make ~mode:Res_parser.Default src filename

let structure = Res_core.parseImplementation p
let signature = Res_core.parseSpecification p

let printError ppf src ({loc; msg} : Location.error) =
  Res_diagnostics_printing_utils.Super_location.setup_colors ();
  (* open a vertical box. Everything in our message is indented 2 spaces *)
  (* Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]" (print ~message_kind:`error "We've found a bug for you!") src loc msg; *)
  Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]"
    (Res_diagnostics_printing_utils.Super_location.print ~message_kind:`error "Lint error!" src)
    loc msg

let printError src msg d =
  printError
    Format.err_formatter src
    Location.
      {
        loc = {loc_start = d.loc_start; loc_end = d.loc_end; loc_ghost = false};
        msg = msg;
        sub = [];
        if_highlight = "";
      }

let withStructure _src iterator f =
  {
    iterator with
    Ast_iterator.structure =
       (fun iterator1 structure ->
         let _ = f structure in
         iterator.Ast_iterator.structure iterator1 structure);
  }

let withExpression src iterator f =
  {
    iterator with
    Ast_iterator.expr =
       (fun iterator1 expr ->
         let res = f expr in
         (match res with
         | Rule.LintError(msg, loc) -> printError src msg loc
         | Rule.LintOk -> ()
         );
         iterator.Ast_iterator.expr iterator1 expr);
  }


module DisallowStringOfIntRule = DisallowedFunctionRule.Make(struct
  type options = DisallowedFunctionRule.Options.options
  let options =
    { DisallowedFunctionRule.Options.disallowed_function = "string_of_int"
    ; DisallowedFunctionRule.Options.suggested_function = Some "Belt.Int.fromString"
    }
end)

module DisallowIntOfStringOptRule = DisallowedFunctionRule.Make(struct
  type options = DisallowedFunctionRule.Options.options
  let options =
    { DisallowedFunctionRule.Options.disallowed_function = "intOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function = Some "Belt.Int.fromString"
    }
end)


module DisallowFloatOfStringOptRule = DisallowedFunctionRule.Make(struct
  type options = DisallowedFunctionRule.Options.options
  let options =
    { DisallowedFunctionRule.Options.disallowed_function = "floatOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function = Some "Belt.Float.fromString"
    }
end)

let rules =
  [ (module DisallowStringOfIntRule : Rule.HASRULE)
  ; (module DisallowIntOfStringOptRule : Rule.HASRULE)
  ; (module DisallowFloatOfStringOptRule : Rule.HASRULE)
  ]

let makeIterator p =
  let f iterator rule =
    let module R = (val rule : Rule.HASRULE) in
    (match R.proxy with
    | Rule.MExpression -> withExpression p.Res_parser.scanner.src iterator R.lint
    | Rule.MStructure -> withStructure p.Res_parser.scanner.src iterator R.lint
    ) in
  List.fold_left f Ast_iterator.default_iterator rules

let run = match p.diagnostics with
| [] ->
    let iterator = makeIterator p in
    iterator.structure iterator structure
| diagnostics -> (* parser contains problems *)
  Res_diagnostics.printReport diagnostics src
