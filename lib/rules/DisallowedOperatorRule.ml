open Rescript_parser

module Options = struct
  type options = {disallowed_operator: string; suggested_operator: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) :
  Rule.HASRULE with type t = Parsetree.expression = struct
  let description =
    match OPT.options.suggested_operator with
    | Some op -> "[Rescript] Do not use " ^ OPT.options.disallowed_operator ^ ", use " ^ op
    | None -> "[Rescript] Do not use " ^ OPT.options.disallowed_operator

  type t = Parsetree.expression

  let proxy = Rule.MExpression

  let meta = {Rule.ruleName= "Disallowed Operator"; Rule.ruleDescription= description}

  let op = OPT.options.Options.disallowed_operator

  let lint expr =
    match expr with
    (* matches string_of_int(x) *)
    | { Parsetree.pexp_desc= Pexp_apply ({pexp_desc= Pexp_ident {txt= Longident.Lident ident}}, _)
      ; Parsetree.pexp_loc= loc }
      when ident = op ->
        Rule.LintError (meta.ruleDescription, loc)
    | _ -> Rule.LintOk
end
