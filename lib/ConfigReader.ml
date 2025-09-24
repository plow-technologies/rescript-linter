exception RuleDoesNotExist

type t = {rules: (module Rule.HASRULE) list}

let createDisallowOperatorRule options (linterOptions : (module Rule.LinterOptions)) =
  let open Yojson.Basic.Util in
  let disallowed_operator = options |> member "disallowed_operator" |> to_string in
  let suggested_operator = options |> member "suggested_operator" |> to_string in
  let (module LinterOptions) = linterOptions in
  let module M =
    DisallowedOperatorRule.Make
      (struct
        type options = DisallowedOperatorRule.Options.options

        let options =
          { DisallowedOperatorRule.Options.disallowed_operator
          ; DisallowedOperatorRule.Options.suggested_operator= Some suggested_operator }
      end)
      (LinterOptions)
  in
  (module M : Rule.HASRULE)

let createDisallowFunctionRule options (linterOptions : (module Rule.LinterOptions)) =
  let open Yojson.Basic.Util in
  let disallowed_function = options |> member "disallowed_function" |> to_string in
  let suggested_function = options |> member "suggested_function" |> to_string in
  let (module LinterOptions) = linterOptions in
  let module M =
    DisallowedFunctionRule.Make
      (struct
        type options = DisallowedFunctionRule.Options.options

        let options =
          { DisallowedFunctionRule.Options.disallowed_function
          ; DisallowedFunctionRule.Options.suggested_function= Some suggested_function }
      end)
      (LinterOptions)
  in
  (module M : Rule.HASRULE)

let createNoReactComponentRule options (linterOptions : (module Rule.LinterOptions)) =
  let open Yojson.Basic.Util in
  let component_name = options |> member "component" |> to_string in
  let suggested_component_name = options |> member "suggested_component" |> to_string in
  let (module LinterOptions) = linterOptions in
  let module M =
    NoReactComponentRule.Make
      (struct
        type options = NoReactComponentRule.Options.options

        let options =
          { NoReactComponentRule.Options.component_name
          ; NoReactComponentRule.Options.suggested_component_name= Some suggested_component_name }
      end)
      (LinterOptions)
  in
  (module M : Rule.HASRULE)

let createDisallowModuleRule options (linterOptions : (module Rule.LinterOptions)) =
  let open Yojson.Basic.Util in
  let disallowed_module = options |> member "disallowed_module" |> to_string in
  let suggested_module = options |> member "suggested_module" |> to_string in
  let (module LinterOptions) = linterOptions in
  let module M =
    DisallowModuleRule.Make
      (struct
        type options = DisallowModuleRule.Options.options

        let options =
          { DisallowModuleRule.Options.disallowed_module
          ; DisallowModuleRule.Options.suggested_module= Some suggested_module }
      end)
      (LinterOptions)
  in
  (module M : Rule.HASRULE)

let createDisallowEmbeddedRegexLiteralRule options (linterOptions : (module Rule.LinterOptions)) =
  let open Yojson.Basic.Util in
  let test_directory = options |> member "test_directory" |> to_string in
  let (module LinterOptions) = linterOptions in
  let module M =
    DisallowedEmbeddedRegexLiteralRule.Make
      (struct
        type options = DisallowedEmbeddedRegexLiteralRule.Options.options

        let options = {DisallowedEmbeddedRegexLiteralRule.Options.test_directory}
      end)
      (LinterOptions)
  in
  (module M : Rule.HASRULE)

let createDisallowAttributeRule options (linterOptions : (module Rule.LinterOptions)) =
  let open Yojson.Basic.Util in
  let attribute = options |> member "attribute" |> to_string in
  let suggestion =
    match options |> member "suggestion" with `Null -> None | json -> Some (to_string json)
  in
  let (module LinterOptions) = linterOptions in
  let module M =
    DisallowedAttributeRule.Make
      (struct
        type options = DisallowedAttributeRule.Options.options

        let options = {DisallowedAttributeRule.Options.suggestion; DisallowedAttributeRule.Options.attribute}
      end)
      (LinterOptions)
  in
  (module M : Rule.HASRULE)

let parseConfig path =
  let json = Yojson.Basic.from_file path in
  let open Yojson.Basic.Util in
  let filter_rule json =
    (* Parse in the linter options that are common to every rule *)
    let warning =
      match json |> member "warning" with `Null -> false (* default to errors *) | json -> to_bool json
    in
    let linterOptions =
      ( module struct
        let warning = warning
      end : Rule.LinterOptions )
    in
    (* Parse the rule name, this is used to identify the rule being configured
     * as each rule can have different parsing options
     *)
    let rule = json |> member "rule" |> to_string in
    (* Not all rules are explicitly required to have options
       * but perhaps in the future we can clean up the code duplication
       * of parsing options for the rules that do have options
    *)
    match rule with
    | "DisallowOperator" ->
        let options = json |> member "options" in
        createDisallowOperatorRule options linterOptions
    | "DisallowFunction" ->
        let options = json |> member "options" in
        createDisallowFunctionRule options linterOptions
    | "NoReactComponent" ->
        let options = json |> member "options" in
        createNoReactComponentRule options linterOptions
    | "DisallowModule" ->
        let options = json |> member "options" in
        createDisallowModuleRule options linterOptions
    | "DisallowEmbeddedRegexLiteral" ->
        let options = json |> member "options" in
        createDisallowEmbeddedRegexLiteralRule options linterOptions
    | "DisallowAttribute" ->
        let options = json |> member "options" in
        createDisallowAttributeRule options linterOptions
    | _ -> raise RuleDoesNotExist
  in
  json |> member "rules" |> to_list |> List.map filter_rule
