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

  (* Helper function to convert Longident to string *)
  let rec longident_to_string = function
    | Longident.Lident s -> s
    | Longident.Ldot (t, s) -> longident_to_string t ^ "." ^ s
    | Longident.Lapply (a, b) -> longident_to_string a ^ "(" ^ longident_to_string b ^ ")"

  let lintExpression =
    Rule.LintExpression
      (fun expr ->
        match expr with
        (* matches string_of_int(x) or Js.log(x) *)
        | { Parsetree.pexp_desc=
              Pexp_apply
                { funct= {pexp_desc= Pexp_ident {txt= ident}; Parsetree.pexp_loc= loc}
                ; args= _ } }
          when longident_to_string ident = function_name ->
            Rule.LintError (meta.ruleDescription, loc)
        (* matches x->string_of_int or x->Js.log *)
        | {Parsetree.pexp_desc= Pexp_apply {args= xs; _}; Parsetree.pexp_loc= loc} -> (
            let f expr =
              match expr with
              | Asttypes.Nolabel, {Parsetree.pexp_desc= Pexp_ident {txt= ident}}
                when longident_to_string ident = function_name ->
                  true
              | _ -> false
            in
            let results = List.find_all f xs in
            match results with [] -> Rule.LintOk | _ -> Rule.LintError (meta.ruleDescription, loc) )
        | _ -> Rule.LintOk )

  let linters = [lintExpression]
end
