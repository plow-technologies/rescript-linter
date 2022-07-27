open Rescript_parser
open Rescript_linter

module DisallowStringOfIntRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
end)

module DisallowTriangleOperatorRule = DisallowedOperatorRule.Make (struct
  type options = DisallowedOperatorRule.Options.options

  let options =
    { DisallowedOperatorRule.Options.disallowed_operator= "|>"
    ; DisallowedOperatorRule.Options.suggested_operator= Some "->" }
end)

let parseAst path =
  let src = Linter.processFile path in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  let p = Res_parser.make ~mode:Res_parser.Default src path in
  Res_core.parseImplementation p

module Tests = struct
  (* The tests *)
  let disallow_test_1 () =
    let ast = parseAst "testData/disallowed_function_rule_test_1.res" in
    let errors = Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] ast in
    match errors with
    | [(msg, _); _] ->
        Alcotest.(check string) "Same error message" DisallowStringOfIntRule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should only have two lint errors"

  let disallow_test_2 () =
    let ast = parseAst "testData/disallowed_function_rule_test_2.res" in
    let errors = Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] ast in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should not have any lint errors"

  let disallow_operator_test () =
    let ast = parseAst "testData/disallowed_operator_rule_test.res" in
    let errors = Linter.lint [(module DisallowTriangleOperatorRule : Rule.HASRULE)] ast in
    match errors with
    | [(msg, _)] ->
        Alcotest.(check string) "Same error message" DisallowTriangleOperatorRule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should not have any lint errors"

  let no_jstring_interpolation_test () =
    let ast = parseAst "testData/no_jstring_interpolation_test.res" in
    let errors = Linter.lint [(module NoJStringInterpolation.Rule : Rule.HASRULE)] ast in
    match errors with
    | [(msg, _)] ->
        Alcotest.(check pass) "Same error message" NoJStringInterpolation.Rule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should only return one error"
end

(* Run it *)
let () =
  let open Alcotest in
  run "ReScript Linter"
    [ ( "Disallow Function Rule"
      , [ test_case "Lint only functions" `Quick Tests.disallow_test_1
        ; test_case "Does not lint variable with the same function name" `Quick Tests.disallow_test_2 ] )
    ; ( "No J String Interpolation Rule"
      , [test_case "Lint j`` string" `Quick Tests.no_jstring_interpolation_test] )
    ; ("Disallow |> operator", [test_case "Lint |> operator" `Quick Tests.disallow_operator_test]) ]
