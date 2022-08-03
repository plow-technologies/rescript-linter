open Rescript_parser

module Options = struct
  type options = {disallowed_operator: string; suggested_operator: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) :
  Rule.HASRULE with type t = Parsetree.expression = struct
  let description =
    match OPT.options.suggested_operator with
    | Some op ->
        "[Rescript] Do not use " ^ OPT.options.disallowed_operator ^ ", please use " ^ op ^ " instead."
    | None -> "[Rescript] Do not use " ^ OPT.options.disallowed_operator

  type t = Parsetree.expression

  let proxy = Rule.MExpression

  let op = OPT.options.Options.disallowed_operator

  let meta =
    { Rule.ruleName= "DisallowOperator"
    ; Rule.ruleIdentifier= "DisallowOperator" ^ "[" ^ op ^ "]"
    ; Rule.ruleDescription= description }

  let lint expr =
    match expr with
    (* matches string_of_int(x) *)
    | { Parsetree.pexp_desc=
          Pexp_apply ({pexp_desc= Pexp_ident {txt= Longident.Lident ident}; Parsetree.pexp_loc= loc}, _) }
      when ident = op ->
        Rule.LintError (meta.ruleDescription, loc)
    | _ -> Rule.LintOk
end
