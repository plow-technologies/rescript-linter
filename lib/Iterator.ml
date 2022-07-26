open Rescript_parser

let withStructure iterator f _callback =
  { iterator with
    Ast_iterator.structure=
      (fun iterator1 structure ->
        let _ = f structure in
        iterator.Ast_iterator.structure iterator1 structure ) }

let withExpression iterator f callback =
  { iterator with
    Ast_iterator.expr=
      (fun iterator1 expr ->
        let res = f expr in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.expr iterator1 expr ) }

let makeIterator rules callback =
  let f iterator rule =
    let module R = (val rule : Rule.HASRULE) in
    match R.proxy with
    | Rule.MExpression -> withExpression iterator R.lint callback
    | Rule.MStructure -> withStructure iterator R.lint callback
  in
  List.fold_left f Ast_iterator.default_iterator rules
