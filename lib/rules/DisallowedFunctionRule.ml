module Options = struct
  type options = {disallowed_function: string; suggested_function: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) (LinterOptions : Rule.LinterOptions) :
  Rule.HASRULE = struct
  include LinterOptions

  let description =
    match OPT.options.suggested_function with
    | Some func_name ->
        "[Rescript] Do not use " ^ OPT.options.disallowed_function ^ ", please use " ^ func_name ^ " instead."
    | None -> "[Rescript] Do not use " ^ OPT.options.disallowed_function

  let function_name = OPT.options.Options.disallowed_function

  let meta =
    { Rule.ruleName= "DisallowFunction"
    ; Rule.ruleIdentifier= "DisallowFunction" ^ "[" ^ function_name ^ "]"
    ; Rule.ruleDescription= description }

  let lintExpression =
    Rule.LintExpression
      (fun expr ->
        match expr with
        (* matches string_of_int(x) *)
        | { Parsetree.pexp_desc=
              Pexp_apply
                { funct= {pexp_desc= Pexp_ident {txt= Longident.Lident ident}; Parsetree.pexp_loc= loc}
                ; args= _ } }
          when ident = function_name ->
            Rule.LintError (meta.ruleDescription, loc)
        (* matches x->string_of_int *)
        | {Parsetree.pexp_desc= Pexp_apply {args= xs; _}; Parsetree.pexp_loc= loc} -> (
            let f expr =
              match expr with
              | Asttypes.Nolabel, {Parsetree.pexp_desc= Pexp_ident {txt= Longident.Lident ident}}
                when ident = function_name ->
                  true
              | _ -> false
            in
            let results = List.find_all f xs in
            match results with [] -> Rule.LintOk | _ -> Rule.LintError (meta.ruleDescription, loc) )
        | _ -> Rule.LintOk )

  let linters = [lintExpression]
end
