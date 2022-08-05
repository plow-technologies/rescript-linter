open Rescript_parser

module Rule : Rule.HASRULE = struct
  let meta =
    { Rule.ruleName= "NoJStringInterpolation"
    ; Rule.ruleIdentifier= "NoJStringInterpolation"
    ; Rule.ruleDescription=
        "[Rescript] Do not use j`<string>` interpolation, use `` instead and explicitly convert args to \
         string." }

  let lintExpresion =
    Rule.LintExpression
      (fun expr ->
        match expr with
        | {Parsetree.pexp_desc= Pexp_constant (Parsetree.Pconst_string (_, Some "j")); Parsetree.pexp_loc= loc}
          ->
            Rule.LintError (meta.ruleDescription, loc)
        | _ -> Rule.LintOk )

  let linters = [lintExpresion]
end

include Rule
