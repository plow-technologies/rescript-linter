open Rescript_parser

module Options = struct
  type options = {disallowed_module: string; suggested_module: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) : Rule.HASRULE = struct
  let description =
    match OPT.options.suggested_module with
    | Some op -> "[Rescript] Do not use " ^ OPT.options.disallowed_module ^ ", please use " ^ op ^ " instead."
    | None -> "[Rescript] Do not use " ^ OPT.options.disallowed_module

  let op = OPT.options.Options.disallowed_module

  let meta =
    { Rule.ruleName= "DisallowModule"
    ; Rule.ruleIdentifier= "DisallowModule" ^ "[" ^ op ^ "]"
    ; Rule.ruleDescription= description }

  (* There are three cases that we need to handle when linting for module usage (assume M is the module name)

     1. M.function 2. open M 3. module N = M *)
  let lintStructureItem =
    Rule.LintStructureItem
      (fun expr ->
        match expr with
        (* match open M *)
        | {Parsetree.pstr_desc= Parsetree.Pstr_open {Parsetree.popen_lid= {txt= Longident.Lident _ident}}} ->
            Rule.LintOk
        (* match J = M *)
        | { Parsetree.pstr_desc=
              Parsetree.Pstr_module
                { Parsetree.pmb_expr=
                    {Parsetree.pmod_desc= Parsetree.Pmod_ident {txt= Longident.Lident _ident}} } } ->
            Rule.LintOk
        | _ -> Rule.LintOk )

  let lint = [lintStructureItem]
end

(* structure_item (test.res[21,300+2]..[21,300+51]) Pstr_value Nonrec [ <def> pattern
   (test.res[21,300+6]..[21,300+7]) Ppat_any expression (test.res[21,300+10]..[21,300+51]) attribute "bs"
   (_none_[1,0+-1]..[1,0+-1]) ghost [] Pexp_apply expression (test.res[21,300+10]..[21,300+21]) Pexp_ident
   "CssJs.style" (test.res[21,300+10]..[21,300+21]) [ <arg> Nolabel expression
   (test.res[21,300+24]..[21,300+50]) Pexp_array [ expression (test.res[21,300+25]..[21,300+49]) Pexp_apply
   expression (test.res[21,300+25]..[21,300+41]) Pexp_ident "CssJs.marginLeft"
   (test.res[21,300+25]..[21,300+41]) [ <arg> Nolabel expression (test.res[21,300+42]..[21,300+48])
   Pexp_variant "px" Some expression (test.res[21,300+46]..[21,300+47]) Pexp_constant PConst_int (0,None) ] ]
   ] ] ] *)
