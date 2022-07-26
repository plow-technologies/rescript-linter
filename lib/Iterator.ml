open Rescript_parser

let withStructure _src iterator f =
  { iterator with
    Ast_iterator.structure=
      (fun iterator1 structure ->
        let _ = f structure in
        iterator.Ast_iterator.structure iterator1 structure ) }

let withExpression src iterator f =
  { iterator with
    Ast_iterator.expr=
      (fun iterator1 expr ->
        let res = f expr in
        ( match res with
        | Rule.LintError (msg, loc) -> Printer.printError src msg loc
        | Rule.LintOk -> () ) ;
        iterator.Ast_iterator.expr iterator1 expr ) }

let makeIterator p rules =
  let f iterator rule =
    let module R = (val rule : Rule.HASRULE) in
    match R.proxy with
    | Rule.MExpression ->
        withExpression p.Res_parser.scanner.src iterator R.lint
    | Rule.MStructure ->
        withStructure p.Res_parser.scanner.src iterator R.lint
  in
  List.fold_left f Ast_iterator.default_iterator rules
