open Rescript_linter

module TestingLinterOptions = struct
  let warning = false
end

module TestingLinterOptionsWarning = struct
  let warning = true
end

module DisallowStringOfIntRule =
  DisallowedFunctionRule.Make
    (struct
      type options = DisallowedFunctionRule.Options.options

      let options =
        { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
        ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
    end)
    (TestingLinterOptions)

module DisallowStringOfIntRuleWarning =
  DisallowedFunctionRule.Make
    (struct
      type options = DisallowedFunctionRule.Options.options

      let options =
        { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
        ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
    end)
    (TestingLinterOptionsWarning)

module DisallowJsLogRule =
  DisallowedFunctionRule.Make
    (struct
      type options = DisallowedFunctionRule.Options.options

      let options =
        { DisallowedFunctionRule.Options.disallowed_function= "Js.log"
        ; DisallowedFunctionRule.Options.suggested_function= Some "Console.log" }
    end)
    (TestingLinterOptions)

module DisallowInOfStringOptRule =
  DisallowedFunctionRule.Make
    (struct
      type options = DisallowedFunctionRule.Options.options

      let options =
        { DisallowedFunctionRule.Options.disallowed_function= "intOfStringOpt"
        ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
    end)
    (TestingLinterOptions)

module NoInputComponentRule =
  NoReactComponentRule.Make
    (struct
      type options = NoReactComponentRule.Options.options

      let options =
        { NoReactComponentRule.Options.component_name= "input"
        ; NoReactComponentRule.Options.suggested_component_name= None }
    end)
    (TestingLinterOptions)

module NoInnerComponentRule =
  NoReactComponentRule.Make
    (struct
      type options = NoReactComponentRule.Options.options

      let options =
        { NoReactComponentRule.Options.component_name= "Inner"
        ; NoReactComponentRule.Options.suggested_component_name= Some "SafeInner" }
    end)
    (TestingLinterOptions)

module NoCSSModuleRule =
  DisallowModuleRule.Make
    (struct
      type options = DisallowModuleRule.Options.options

      let options =
        { DisallowModuleRule.Options.disallowed_module= "Css"
        ; DisallowModuleRule.Options.suggested_module= Some "CssJS" }
    end)
    (TestingLinterOptions)

module DisallowBeltResultRule =
  DisallowModuleRule.Make
    (struct
      type options = DisallowModuleRule.Options.options

      let options =
        { DisallowModuleRule.Options.disallowed_module= "Belt.Result"
        ; DisallowModuleRule.Options.suggested_module= Some "Result" }
    end)
    (TestingLinterOptions)

module DisallowBeltRule =
  DisallowModuleRule.Make
    (struct
      type options = DisallowModuleRule.Options.options

      let options =
        { DisallowModuleRule.Options.disallowed_module= "Belt"
        ; DisallowModuleRule.Options.suggested_module= Some "Core" }
    end)
    (TestingLinterOptions)

module DisallowedEmbeddedRegexLiteralRule =
  DisallowedEmbeddedRegexLiteralRule.Make
    (struct
      type options = DisallowedEmbeddedRegexLiteralRule.Options.options

      let options = {DisallowedEmbeddedRegexLiteralRule.Options.test_directory= "testData"}
    end)
    (TestingLinterOptions)

module DisallowedAttributeRule =
  DisallowedAttributeRule.Make
    (struct
      type options = DisallowedAttributeRule.Options.options

      let options =
        { DisallowedAttributeRule.Options.attribute= "dead"
        ; DisallowedAttributeRule.Options.suggestion= Some "Remove, refactor, or mark as @live" }
    end)
    (TestingLinterOptions)

type parseResult = {ast: Parsetree.structure; comments: Res_comment.t list}

let loc_to_string (loc : Location.t) : string = Format.asprintf "%a" Location.print_loc loc

let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub = 0 then true (* An empty substring is always "contained" *)
  else if len_sub > len_s then false (* Substring cannot be longer than the main string *)
  else
    let rec check_from_index i =
      if i > len_s - len_sub then false (* No more possible starting positions *)
      else
        let extracted_sub = String.sub s i len_sub in
        if String.equal extracted_sub sub then true else check_from_index (i + 1)
    in
    check_from_index 0

let parseAst path =
  let src = Linter.processFile path in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  let p = Res_parser.make ~mode:Res_parser.Default src path in
  {ast= Res_core.parse_implementation p; comments= p.comments}

module Tests = struct
  (* The tests *)
  let disallow_test_1 () =
    let parseResult = parseAst "testData/disallowed_function_rule_test_1.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [(msg, _); _] ->
        Alcotest.(check string) "Same error message" DisallowStringOfIntRule.meta.ruleDescription msg
    | _ -> Alcotest.fail "Should only have two lint errors"

  let disallow_test_1_warning () =
    let parseResult = parseAst "testData/disallowed_function_rule_test_1.res" in
    let errors, warnings =
      Linter.lint
        [(module DisallowStringOfIntRuleWarning : Rule.HASRULE)]
        parseResult.ast parseResult.comments
    in
    match (errors, warnings) with
    | [], [(msg, _); _] ->
        Alcotest.(check string) "Same error message" DisallowStringOfIntRuleWarning.meta.ruleDescription msg
    | errors, warnings ->
        Alcotest.fail
          ( "Should only have two lint warnings for string_of_int, there were "
          ^ string_of_int (List.length errors)
          ^ " errors found and "
          ^ string_of_int (List.length warnings)
          ^ " warnings found" )

  let disallow_test_2 () =
    let parseResult = parseAst "testData/disallowed_function_rule_test_2.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should not have any lint errors"

  let disable_lint_test () =
    let parseResult = parseAst "testData/disabled_lint_test_1.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only no lint errors"

  let disable_lint_per_rule_test () =
    let parseResult = parseAst "testData/disabled_lint_test_2.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowStringOfIntRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should have no lint errors"

  let disable_lint_per_rule_specific_test () =
    let parseResult = parseAst "testData/disabled_lint_test_3.res" in
    let errors, _warnings =
      Linter.lint
        [(module DisallowStringOfIntRule : Rule.HASRULE); (module DisallowInOfStringOptRule : Rule.HASRULE)]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have one lint error"

  let disabled_multiple_lints_test () =
    let parseResult = parseAst "testData/disabled_multiple_rules_test.res" in
    let errors, _warnings =
      Linter.lint
        [ (module DisallowStringOfIntRule : Rule.HASRULE)
        ; (module DisallowInOfStringOptRule : Rule.HASRULE)
        ; (module DisallowedAttributeRule : Rule.HASRULE) ]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [] -> Alcotest.(check pass) "Same error message" [] []
    | errors ->
        Alcotest.fail
          ("Should have no lint errors, but found:\n\t* " ^ String.concat "\n\t* " (List.map fst errors))

  let no_react_component_test_1 () =
    let parseResult = parseAst "testData/no_react_component_test_1.res" in
    let errors, _warnings =
      Linter.lint [(module NoInputComponentRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let no_react_component_test_2 () =
    let parseResult = parseAst "testData/no_react_component_test_2.res" in
    let errors, _warnings =
      Linter.lint [(module NoInnerComponentRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_module_test_1 () =
    let parseResult = parseAst "testData/disallow_module_test_1.res" in
    let errors, _warnings =
      Linter.lint [(module NoCSSModuleRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_module_test_2 () =
    let parseResult = parseAst "testData/disallow_module_test_2.res" in
    let errors, _warnings =
      Linter.lint [(module NoCSSModuleRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_module_test_3 () =
    let parseResult = parseAst "testData/disallow_module_test_3.res" in
    let errors, _warnings =
      Linter.lint [(module NoCSSModuleRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have two lint error"

  let disallow_qualified_function_test () =
    let parseResult = parseAst "testData/disallowed_function_rule_test_1.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowJsLogRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | errors ->
        Alcotest.fail
          ("Should only have one lint error, but got " ^ string_of_int (List.length errors) ^ " errors")

  let disallow_module_test_4 () =
    let parseResult = parseAst "testData/disallow_module_test_4.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowBeltResultRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _; _; _; _] -> Alcotest.(check pass) "Same error message" [] []
    | errors ->
        Alcotest.fail
          ( "Should have five lint errors (2 Ok + 2 Error in expressions/patterns, plus map function), but \
             got "
          ^ string_of_int (List.length errors)
          ^ " errors" )

  let disallow_module_test_5 () =
    let parseResult = parseAst "testData/disallow_module_test_5.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowBeltRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    match errors with
    | [_; _; _] -> Alcotest.(check pass) "Same error message" [] []
    | errors ->
        Alcotest.fail
          ( "Should have three lint errors (Belt.List, Belt.Array, Belt.Option), but got "
          ^ string_of_int (List.length errors)
          ^ " errors" )

  let disallowed_embedded_regex_literal_test () =
    let parseResult = parseAst "testData/disallowed_embedded_regex_literal_test.res" in
    let errors, _warnings =
      Linter.lint
        [(module DisallowedEmbeddedRegexLiteralRule : Rule.HASRULE)]
        parseResult.ast parseResult.comments
    in
    match errors with
    | [_] -> Alcotest.(check pass) "Same error message" [] []
    | _ -> Alcotest.fail "Should only have one lint error"

  let disallowed_dead_code_test () =
    let parseResult = parseAst "testData/disallowed_dead_code_test.res" in
    let errors, _warnings =
      Linter.lint [(module DisallowedAttributeRule : Rule.HASRULE)] parseResult.ast parseResult.comments
    in
    let expectedDeadTypes =
      [ "[expression]"
      ; "[expression]"
      ; "[constructor]"
      ; "[label]"
      ; "[label]"
      ; "[constructor]"
      ; "[assignment]"
      ; "[pattern]"
      ; "[pattern]"
      ; "[module]"
      ; "[module]"
      ; "[assignment]"
      ; "[type]"
      ; "[label]"
      ; "[label]" ]
    in
    match errors with
    | [] -> Alcotest.fail ("No lint errors: " ^ DisallowedAttributeRule.meta.ruleDescription)
    | errors when List.length errors = List.length expectedDeadTypes -> (
      match
        List.combine errors expectedDeadTypes
        |> List.for_all (fun ((errString, _errLoc), expected) -> contains_substring errString expected)
      with
      | true -> Alcotest.(check pass) "Same error message" [] []
      | false ->
          Alcotest.fail
            ( "Dead Code Errors: \nExpected errors in this order [" ^ String.concat ", " expectedDeadTypes
            ^ "], but got "
            ^ string_of_int (List.length errors)
            ^ "\n"
            ^ String.concat "\n"
                (List.map
                   (fun (str, loc) -> Printf.sprintf "\t* %s\n\t\t -> %s" str (loc_to_string loc))
                   errors ) ) )
    | errors ->
        Alcotest.fail
          ( "Dead Code Errors: \nExpected "
          ^ string_of_int (List.length expectedDeadTypes)
          ^ " errors, but got "
          ^ string_of_int (List.length errors)
          ^ "\n"
          ^ String.concat "\n"
              (List.map
                 (fun (str, loc) -> Printf.sprintf "\t* %s\n\t\t -> %s" str (loc_to_string loc))
                 errors ) )
end

(* Run it *)
let () =
  let open Alcotest in
  run "ReScript Linter"
    [ ( "Disallow Function Rule"
      , [ test_case "Lint only functions" `Quick Tests.disallow_test_1
        ; test_case "Does not lint variable with the same function name" `Quick Tests.disallow_test_2
        ; test_case "Lint qualified functions (Js.log)" `Quick Tests.disallow_qualified_function_test ] )
    ; ( "Warning Lint Rule"
      , [test_case "Lint only functions (as warning)" `Quick Tests.disallow_test_1_warning] )
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
        ; test_case "direct access module" `Quick Tests.disallow_module_test_3
        ; test_case "qualified module with constructors (Belt.Result)" `Quick Tests.disallow_module_test_4
        ; test_case "module prefix matching (Belt.*)" `Quick Tests.disallow_module_test_5 ] )
    ; ( "Disallowed embedded regex literal"
      , [test_case "Disallowed embedded regex literal" `Quick Tests.disallowed_embedded_regex_literal_test] )
    ; ("Disallowed dead code", [test_case "Disallowed dead code" `Quick Tests.disallowed_dead_code_test]) ]
