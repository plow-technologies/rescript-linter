open Rescript_parser

module Options = struct
  type options = {component_name: string}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) :
  Rule.HASRULE with type t = Parsetree.expression = struct
  type t = Parsetree.expression

  let proxy = Rule.MExpression

  let component_name = OPT.options.Options.component_name

  let meta =
    { Rule.ruleName= "NoReactComponent"
    ; Rule.ruleIdentifier= "NoReactComponent" ^ "[" ^ component_name ^ "]"
    ; Rule.ruleDescription= "Don't use " ^ component_name ^ " component." }

  let lint expr =
    match expr with
    (* Parse React dom like <input /> *)
    | { Parsetree.pexp_desc= Pexp_apply ({pexp_desc= Pexp_ident {txt= Longident.Lident ident}}, _)
      ; Parsetree.pexp_attributes= [({Asttypes.txt= "JSX"}, _)]
      ; Parsetree.pexp_loc= loc }
      when ident = component_name ->
        print_endline ident ;
        Rule.LintError (meta.ruleDescription, loc)
    (* Parse custom React component like <Component /> *)
    | { Parsetree.pexp_desc=
          Pexp_apply
            ({pexp_desc= Pexp_ident {txt= Longident.Ldot (Longident.Lident ident, "createElement")}}, _)
      ; Parsetree.pexp_attributes= [({Asttypes.txt= "JSX"}, _)]
      ; Parsetree.pexp_loc= loc }
      when ident = component_name ->
        Rule.LintError (meta.ruleDescription, loc)
    | _ -> Rule.LintOk
end
