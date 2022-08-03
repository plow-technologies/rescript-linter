open Rescript_parser

module Rule : Rule.HASRULE with type t = Parsetree.expression = struct
  type t = Parsetree.expression

  let proxy = Rule.MExpression

  let meta =
    { Rule.ruleName= "NoJStringInterpolation"
    ; Rule.ruleIdentifier= "NoJStringInterpolation"
    ; Rule.ruleDescription=
        "[Rescript] Do not use j`<string>` interpolation, use `` instead and explicitly convert args to \
         string." }

  let lint expr =
    match expr with
    | {Parsetree.pexp_desc= Pexp_constant (Parsetree.Pconst_string (_, Some "j")); Parsetree.pexp_loc= loc} ->
        Rule.LintError (meta.ruleDescription, loc)
    | _ -> Rule.LintOk
end
