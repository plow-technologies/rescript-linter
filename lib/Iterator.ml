let withPattern iterator f callback =
  { iterator with
    Ast_iterator.pat=
      (fun iterator1 pattern ->
        let res = f pattern in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.pat iterator1 pattern ) }

let withStructure iterator f callback =
  { iterator with
    Ast_iterator.structure=
      (fun iterator1 structure ->
        let res = f structure in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.structure iterator1 structure ) }

let withStructureItem iterator f callback =
  { iterator with
    Ast_iterator.structure_item=
      (fun iterator1 structure ->
        let res = f structure in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.structure_item iterator1 structure ) }

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
    let buildIterator iterator lint =
      match lint with
      | Rule.LintExpression lintFunc -> withExpression iterator lintFunc callback
      | Rule.LintStructure lintFunc -> withStructure iterator lintFunc callback
      | Rule.LintStructureItem lintFunc -> withStructureItem iterator lintFunc callback
      | Rule.LintPattern lintFunc -> withPattern iterator lintFunc callback
    in
    List.fold_left buildIterator iterator R.linters
  in
  List.fold_left f Ast_iterator.default_iterator rules
