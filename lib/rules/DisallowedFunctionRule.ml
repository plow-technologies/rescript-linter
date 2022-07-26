open Rescript_parser

module Rule : Rule.HASRULE with type t = Parsetree.expression = struct
  type t = Parsetree.expression
  let proxy = Rule.MExpression
  let meta =
    { Rule.ruleName = "DisallowedFunction"
    ; Rule.ruleDescription = "[Rescript] Do not use int_of_string, use Belt.Int.fromString instead"
    }

  let lint expr =
        (match expr with
        (* matches string_of_int(x) *)
        | {Parsetree.pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident "string_of_int"}}, _)} ->
            print_endline meta.ruleDescription;
            Rule.LintOk
        (* matches x->string_of_int *)
        | {Parsetree.pexp_desc = Pexp_apply (_, xs)} ->
            let f expr =
              (match expr with
              | (Asttypes.Nolabel, {Parsetree.pexp_desc = Pexp_ident {txt = Longident.Lident "string_of_int"}}) -> true
              | _ -> false
              ) in
            let results = List.find_all f xs in
            (match results with
            | [] -> Rule.LintOk
            | _ ->
                print_endline meta.ruleDescription;
                Rule.LintOk
            );
        | _ -> Rule.LintOk
        );
end
