module Options = struct
  type options = {disallowed_module: string; suggested_module: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) (LinterOptions : Rule.LinterOptions) :
  Rule.HASRULE = struct
  include LinterOptions

  let description =
    match OPT.options.suggested_module with
    | Some op ->
        "[Rescript] Do not use module " ^ OPT.options.disallowed_module ^ ", please use module " ^ op
        ^ " instead."
    | None -> "[Rescript] Do not use " ^ OPT.options.disallowed_module

  let op = OPT.options.Options.disallowed_module

  let meta =
    { Rule.ruleName= "DisallowModule"
    ; Rule.ruleIdentifier= "DisallowModule" ^ "[" ^ op ^ "]"
    ; Rule.ruleDescription= description }

  (* There are three cases that we need to handle when linting for module usage (assume M is the module name)

     1. open M
     2. module N = M
     3. M.function or M.attribute

     For 3, we might have false positive because the lint looks for `Pexp_ident` with the module name.

     For example, if you something like

     ```rescript
     // shadow existing module CssJs
     module CssJs = {
       let func () = ...
     }

     let _ = CssJs.func()
     ```

     This will trigger lint error on line `let _ = CssJs.func()`. Anyway I don't think this is a good pattern anyway to shadow existing
     module.
  *)
  let lintStructureItem =
    Rule.LintStructureItem
      (fun expr ->
        match expr with
        (* match open M *)
        | { Parsetree.pstr_desc=
              Parsetree.Pstr_open
                {Parsetree.popen_lid= {txt= Longident.Lident ident}; Parsetree.popen_loc= loc} }
          when ident = op ->
            Rule.LintError (meta.ruleDescription, loc)
        (* match J = M *)
        | { Parsetree.pstr_desc=
              Parsetree.Pstr_module
                { Parsetree.pmb_expr=
                    { Parsetree.pmod_desc= Parsetree.Pmod_ident {txt= Longident.Lident ident}
                    ; Parsetree.pmod_loc= loc } } }
          when ident = op ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let lintExpression =
    Rule.LintExpression
      (fun expr ->
        match expr with
        (* match M.function or M.attribute *)
        | { Parsetree.pexp_desc=
              Pexp_apply
                ( { pexp_desc= Pexp_ident {txt= Longident.Ldot (Longident.Lident ident, _)}
                  ; Parsetree.pexp_loc= loc }
                , _ ) }
          when ident = op ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let linters = [lintStructureItem; lintExpression]
end
