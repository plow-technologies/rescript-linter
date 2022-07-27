open Rescript_parser

module Rule : Rule.HASRULE with type t = Parsetree.expression = struct
  type t = Parsetree.expression

  let proxy = Rule.MExpression

  let meta = {Rule.ruleName= "No input type"; Rule.ruleDescription= "Don't use input type"}

  let lint expr =
    match expr with
    | { Parsetree.pexp_desc= Pexp_apply ({pexp_desc= Pexp_ident {txt= Longident.Lident "input"}}, _)
      ; Parsetree.pexp_attributes= [({Asttypes.txt= "JSX"}, _)]
      ; Parsetree.pexp_loc= loc } ->
        Rule.LintError (meta.ruleDescription, loc)
    | _ -> Rule.LintOk
end
