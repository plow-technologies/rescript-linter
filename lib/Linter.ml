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

let parse acc item =
  match item with
  | () -> acc

let getIterator _callback =
  {
    Ast_iterator.default_iterator with
    structure =
      (fun iterator structure -> Ast_iterator.default_iterator.structure iterator structure);
    expr =
      (fun iterator expr ->
        (match expr with
        | {pexp_desc = Pexp_constant (Parsetree.Pconst_integer(_, _))} -> print_endline "Found integer constant"
        | _ -> ()
        );
        Ast_iterator.default_iterator.expr iterator expr);
  }

let withStructure iterator f =
  {
    iterator with
    Ast_iterator.structure =
       (fun iterator1 structure ->
         let _ = f structure in
         iterator.Ast_iterator.structure iterator1 structure);
  }

let withExpression iterator f =
  {
    iterator with
    Ast_iterator.expr =
       (fun iterator1 expr ->
         let _ = f expr in
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
  ; (module DisallowStringOfIntRule : Rule.HASRULE)
  ; (module DisallowFloatOfStringOptRule : Rule.HASRULE)
  ]

let makeIterator =
  let f iterator rule =
    let module R = (val rule : Rule.HASRULE) in
    (match R.proxy with
    | Rule.MExpression -> withExpression iterator R.lint
    | Rule.MStructure -> withStructure iterator R.lint
    ) in
  List.fold_left f Ast_iterator.default_iterator rules

let run = match p.diagnostics with
| [] ->
    let iterator = makeIterator in
    let () = iterator.structure iterator structure in
    print_endline "No problem"
| diagnostics -> (* parser contains problems *)
  Res_diagnostics.printReport diagnostics src
