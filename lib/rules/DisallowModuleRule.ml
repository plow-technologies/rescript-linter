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

  (* Helper function to convert Longident to string *)
  let rec longident_to_string = function
    | Longident.Lident s -> s
    | Longident.Ldot (t, s) -> longident_to_string t ^ "." ^ s
    | Longident.Lapply (a, b) -> longident_to_string a ^ "(" ^ longident_to_string b ^ ")"

  (* Extract the module path from a Longident (everything except the last component) *)
  let extract_module_path = function
    | Longident.Lident _ -> None
    | Longident.Ldot (t, _) -> Some (longident_to_string t)
    | Longident.Lapply (_, _) -> None

  (* Check if a module path matches the disallowed module
     - Exact match: "Belt" matches "Belt"
     - Prefix match: "Belt" matches "Belt.List", "Belt.Array", etc.
     - But "Belt" should NOT match "BeltExtra"
  *)
  let matches_disallowed_module module_path =
    module_path = op
    || (String.length module_path > String.length op
       && String.sub module_path 0 (String.length op) = op
       && String.get module_path (String.length op) = '.')

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
        (* match open M or open M.N *)
        | { Parsetree.pstr_desc=
              Parsetree.Pstr_open {Parsetree.popen_lid= {txt= ident}; Parsetree.popen_loc= loc} }
          when matches_disallowed_module (longident_to_string ident) ->
            Rule.LintError (meta.ruleDescription, loc)
        (* match J = M or J = M.N *)
        | { Parsetree.pstr_desc=
              Parsetree.Pstr_module
                { Parsetree.pmb_expr= {Parsetree.pmod_desc= Parsetree.Pmod_ident {txt= ident}; Parsetree.pmod_loc= loc} } }
          when matches_disallowed_module (longident_to_string ident) ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let lintExpression =
    Rule.LintExpression
      (fun expr ->
        match expr with
        (* match M.function or M.N.function in function calls *)
        | { Parsetree.pexp_desc=
              Pexp_apply
                {funct= {pexp_desc= Pexp_ident {txt= ident}; Parsetree.pexp_loc= loc}; args= _} }
          when
            ( match extract_module_path ident with
            | Some module_path -> matches_disallowed_module module_path
            | None -> false ) ->
            Rule.LintError (meta.ruleDescription, loc)
        (* match M.Constructor or M.N.Constructor like Belt.Result.Ok *)
        | {Parsetree.pexp_desc= Pexp_construct ({txt= ident; _}, _); Parsetree.pexp_loc= loc}
          when
            ( match extract_module_path ident with
            | Some module_path -> matches_disallowed_module module_path
            | None -> false ) ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let lintPattern =
    Rule.LintPattern
      (fun pat ->
        match pat with
        (* match M.Constructor or M.N.Constructor in patterns like Belt.Result.Ok(x) *)
        | {Parsetree.ppat_desc= Ppat_construct ({txt= ident; _}, _); Parsetree.ppat_loc= loc}
          when
            ( match extract_module_path ident with
            | Some module_path -> matches_disallowed_module module_path
            | None -> false ) ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let linters = [lintStructureItem; lintExpression; lintPattern]
end
