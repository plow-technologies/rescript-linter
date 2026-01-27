module Options = struct
  type options = {component_name: string; suggested_component_name: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) (LinterOptions : Rule.LinterOptions) :
  Rule.HASRULE = struct
  include LinterOptions

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
        (* Parse React dom like <input /> (unary element) *)
        | { Parsetree.pexp_desc=
              Pexp_jsx_element (Jsx_unary_element {jsx_unary_element_tag_name= {txt= JsxLowerTag name; _}; _})
          ; Parsetree.pexp_loc= loc }
          when name = disallowed_component_name ->
            Rule.LintError (meta.ruleDescription, loc)
        (* Parse React dom like <input>...</input> (container element) *)
        | { Parsetree.pexp_desc=
              Pexp_jsx_element
                (Jsx_container_element {jsx_container_element_tag_name_start= {txt= JsxLowerTag name; _}; _})
          ; Parsetree.pexp_loc= loc }
          when name = disallowed_component_name ->
            Rule.LintError (meta.ruleDescription, loc)
        (* Parse custom React component like <Component /> (unary element) *)
        | { Parsetree.pexp_desc=
              Pexp_jsx_element (Jsx_unary_element {jsx_unary_element_tag_name= {txt= JsxUpperTag path; _}; _})
          ; Parsetree.pexp_loc= loc }
          when String.concat "." (Longident.flatten path) = disallowed_component_name ->
            Rule.LintError (meta.ruleDescription, loc)
        (* Parse custom React component like <Component>...</Component> (container element) *)
        | { Parsetree.pexp_desc=
              Pexp_jsx_element
                (Jsx_container_element {jsx_container_element_tag_name_start= {txt= JsxUpperTag path; _}; _})
          ; Parsetree.pexp_loc= loc }
          when String.concat "." (Longident.flatten path) = disallowed_component_name ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let linters = [lintExpression]
end
