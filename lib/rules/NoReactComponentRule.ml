module Options = struct
  type options = {component_name: string; suggested_component_name: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) : Rule.HASRULE = struct
  let disallowed_component_name = OPT.options.Options.component_name

  let optional_suggested_component_name = OPT.options.Options.suggested_component_name

  let description =
    match optional_suggested_component_name with
    | Some suggested_component_name ->
        "[Rescript] Don't use " ^ disallowed_component_name ^ " component, please use "
        ^ suggested_component_name ^ " instead."
    | None -> "[Rescript] Don't use " ^ disallowed_component_name ^ " component."

  let meta =
    { Rule.ruleName= "NoReactComponent"
    ; Rule.ruleIdentifier= "NoReactComponent" ^ "[" ^ disallowed_component_name ^ "]"
    ; Rule.ruleDescription= description }

  let lintExpression =
    Rule.LintExpression
      (fun expr ->
        match expr with
        (* Parse React dom like <input /> *)
        | { Parsetree.pexp_desc= Pexp_apply ({pexp_desc= Pexp_ident {txt= Longident.Lident ident}}, _)
          ; Parsetree.pexp_attributes= [({Asttypes.txt= "JSX"}, _)]
          ; Parsetree.pexp_loc= loc }
          when ident = disallowed_component_name ->
            print_endline ident ;
            Rule.LintError (meta.ruleDescription, loc)
        (* Parse custom React component like <Component /> *)
        | { Parsetree.pexp_desc=
              Pexp_apply
                ({pexp_desc= Pexp_ident {txt= Longident.Ldot (Longident.Lident ident, "createElement")}}, _)
          ; Parsetree.pexp_attributes= [({Asttypes.txt= "JSX"}, _)]
          ; Parsetree.pexp_loc= loc }
          when ident = disallowed_component_name ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let linters = [lintExpression]
end
