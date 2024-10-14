open Rescript_parser
open Rescript_linter

module DisallowStringOfIntRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
end)

module DisallowInOfStringOptRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "intOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
end)

module DisallowTriangleOperatorRule = DisallowedOperatorRule.Make (struct
  type options = DisallowedOperatorRule.Options.options

  let options =
    { DisallowedOperatorRule.Options.disallowed_operator= "|>"
    ; DisallowedOperatorRule.Options.suggested_operator= Some "->" }
end)

module NoInputComponentRule = NoReactComponentRule.Make (struct
  type options = NoReactComponentRule.Options.options

  let options =
    { NoReactComponentRule.Options.component_name= "input"
    ; NoReactComponentRule.Options.suggested_component_name= None }
end)

module NoInnerComponentRule = NoReactComponentRule.Make (struct
  type options = NoReactComponentRule.Options.options

  let options =
    { NoReactComponentRule.Options.component_name= "Inner"
    ; NoReactComponentRule.Options.suggested_component_name= Some "SafeInner" }
end)

module NoCSSModuleRule = DisallowModuleRule.Make (struct
  type options = DisallowModuleRule.Options.options

  let options =
    { DisallowModuleRule.Options.disallowed_module= "Css"
    ; DisallowModuleRule.Options.suggested_module= Some "CssJS" }
end)

module DisallowedEmbeddedRegexLiteralRule = DisallowedEmbeddedRegexLiteralRule.Make (struct
  type options = DisallowedEmbeddedRegexLiteralRule.Options.options

  let options = {DisallowedEmbeddedRegexLiteralRule.Options.test_directory= "testData"}
end)

type parseResult = {ast: Parsetree.structure; comments: Res_comment.t list}

let parseAst path =
  let src = Linter.processFile path in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  let p = Res_parser.make ~mode:Res_parser.Default src path in
  {ast= Res_core.parseImplementation p; comments= p.comments}

module Tests = struct
  (* The tests *)
  let disallow_test_1 () =
    let parseResult = parseAst "testData/disallowed_function_rule_test_1.res" in
    let errors =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [(msg, _); _] ->
        Alcotest.(check string) "Same error message" DisallowStringOfIntRule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should only have two lint errors"

  let disallow_test_2 () =
    let parseResult = parseAst "testData/disallowed_function_rule_test_2.res" in
    let errors =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should not have any lint errors"

  let disallow_operator_test () =
    let parseResult = parseAst "testData/disallowed_operator_rule_test.res" in
    let errors =
      Linter.lint [(module DisallowTriangleOperatorRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [(msg, _)] ->
        Alcotest.(check string) "Same error message" DisallowTriangleOperatorRule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should not have any lint errors"

  let no_jstring_interpolation_test () =
    let parseResult = parseAst "testData/no_jstring_interpolation_test.res" in
    let errors =
      Linter.lint [(module NoJStringInterpolationRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [(msg, _)] ->
        Alcotest.(check pass) "Same error message" NoJStringInterpolationRule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should only return one error"

  let disable_lint_test () =
    let parseResult = parseAst "testData/disabled_lint_test_1.res" in
    let errors =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only no lint errors"

  let disable_lint_per_rule_test () =
    let parseResult = parseAst "testData/disabled_lint_test_2.res" in
    let errors =
      Linter.lint
        [(module DisallowStringOfIntRule : Rule.HASRULE); (module DisallowTriangleOperatorRule : Rule.HASRULE)]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have one lint error"

  let disable_lint_per_rule_specific_test () =
    let parseResult = parseAst "testData/disabled_lint_test_3.res" in
    let errors =
      Linter.lint
        [ (module DisallowStringOfIntRule : Rule.HASRULE)
        ; (module DisallowInOfStringOptRule : Rule.HASRULE)
        ; (module DisallowTriangleOperatorRule : Rule.HASRULE) ]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disabled_multiple_lints_test () =
    let parseResult = parseAst "testData/disabled_multiple_rules_test.res" in
    let errors =
      Linter.lint
        [ (module DisallowStringOfIntRule : Rule.HASRULE)
        ; (module DisallowInOfStringOptRule : Rule.HASRULE)
        ; (module DisallowTriangleOperatorRule : Rule.HASRULE) ]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [(msg, _)] ->
        Alcotest.(check string) "Same error message" msg DisallowTriangleOperatorRule.meta.ruleDescription
    | _ -> Alcotest.fail "Should only have two lint error"

  let no_react_component_test_1 () =
    let parseResult = parseAst "testData/no_react_component_test_1.res" in
    let errors =
      Linter.lint [(module NoInputComponentRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let no_react_component_test_2 () =
    let parseResult = parseAst "testData/no_react_component_test_2.res" in
    let errors =
      Linter.lint [(module NoInnerComponentRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_module_test_1 () =
    let parseResult = parseAst "testData/disallow_module_test_1.res" in
    let errors = Linter.lint [(module NoCSSModuleRule : Rule.HASRULE)] parseResult.ast parseResult.comments in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_module_test_2 () =
    let parseResult = parseAst "testData/disallow_module_test_2.res" in
    let errors = Linter.lint [(module NoCSSModuleRule : Rule.HASRULE)] parseResult.ast parseResult.comments in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_module_test_3 () =
    let parseResult = parseAst "testData/disallow_module_test_3.res" in
    let errors = Linter.lint [(module NoCSSModuleRule : Rule.HASRULE)] parseResult.ast parseResult.comments in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallowed_embedded_regex_literal_test () =
    let parseResult = parseAst "testData/disallowed_embedded_regex_literal_test.res" in
    let errors =
      Linter.lint
        [(module DisallowedEmbeddedRegexLiteralRule : Rule.HASRULE)]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have one lint error"
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
    ; ("Disallow |> operator", [test_case "Lint |> operator" `Quick Tests.disallow_operator_test])
    ; ( "Disable lint test"
      , [ test_case "Disable lint" `Quick Tests.disable_lint_test
        ; test_case "Disable lint per rule" `Quick Tests.disable_lint_per_rule_test
        ; test_case "Disable lint per specific" `Quick Tests.disable_lint_per_rule_specific_test
        ; test_case "Disable multiple lints" `Quick Tests.disabled_multiple_lints_test ] )
    ; ( "No react component"
      , [ test_case "No input box" `Quick Tests.no_react_component_test_1
        ; test_case "No Inner component" `Quick Tests.no_react_component_test_2 ] )
    ; ( "Disallow module"
      , [ test_case "open module" `Quick Tests.disallow_module_test_1
        ; test_case "alias module" `Quick Tests.disallow_module_test_2
        ; test_case "direct access module" `Quick Tests.disallow_module_test_3 ] )
    ; ( "Disallowed embedded regex literal"
      , [test_case "Disallowed embedded regex literal" `Quick Tests.disallowed_embedded_regex_literal_test] )
    ]
