open Rescript_parser

module Rule : Rule.HASRULE with type t = Parsetree.expression = struct
  type t = Parsetree.expression
  let proxy = Rule.MExpression
  let meta =
    { Rule.ruleName = "DisallowedFunction"
    ; Rule.ruleDescription = "Disallow certain functions from running"
    }

  let lint expr =
        (match expr with
        | {Parsetree.pexp_desc = Pexp_constant (Parsetree.Pconst_integer(_, _))} ->
            print_endline "Found integer constant";
            Rule.LintOk
        | _ -> Rule.LintOk
        );
end
