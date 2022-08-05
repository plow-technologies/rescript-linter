open Rescript_parser

module Options = struct
  type options = {disallowed_module: string; suggested_module: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) :
  Rule.HASRULE with type t = Parsetree.structure_item = struct
  let description =
    match OPT.options.suggested_module with
    | Some op -> "[Rescript] Do not use " ^ OPT.options.disallowed_module ^ ", please use " ^ op ^ " instead."
    | None -> "[Rescript] Do not use " ^ OPT.options.disallowed_module

  type t = Parsetree.structure_item

  let proxy = Rule.MStructureItem

  let op = OPT.options.Options.disallowed_module

  let meta =
    { Rule.ruleName= "DisallowModule"
    ; Rule.ruleIdentifier= "DisallowModule" ^ "[" ^ op ^ "]"
    ; Rule.ruleDescription= description }

  (* There are three cases that we need to handle when linting for module usage (assume M is the module name)

     1. M.function 2. open M 3. module N = M *)
  let lint expr =
    match expr with
    (* match open M *)
    | {Parsetree.pstr_desc= Parsetree.Pstr_open {Parsetree.popen_lid= {txt= Longident.Lident _ident}}} ->
        Rule.LintOk
    (* match J = M *)
    | { Parsetree.pstr_desc=
          Parsetree.Pstr_module
            {Parsetree.pmb_expr= {Parsetree.pmod_desc= Parsetree.Pmod_ident {txt= Longident.Lident _ident}}}
      } ->
        Rule.LintOk
    | _ -> Rule.LintOk
end
