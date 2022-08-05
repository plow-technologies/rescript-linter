exception RuleDoesNotExist

type t = {rules: (module Rule.HASRULE) list}

let createDisallowOperatorRule options =
  let open Yojson.Basic.Util in
  let disallowed_operator = options |> member "disallowed_operator" |> to_string in
  let suggested_operator = options |> member "suggested_operator" |> to_string in
  let module M = DisallowedOperatorRule.Make (struct
    type options = DisallowedOperatorRule.Options.options

    let options =
      { DisallowedOperatorRule.Options.disallowed_operator
      ; DisallowedOperatorRule.Options.suggested_operator= Some suggested_operator }
  end) in
  (module M : Rule.HASRULE)

let createDisallowFunctionRule options =
  let open Yojson.Basic.Util in
  let disallowed_function = options |> member "disallowed_function" |> to_string in
  let suggested_function = options |> member "suggested_function" |> to_string in
  let module M = DisallowedFunctionRule.Make (struct
    type options = DisallowedFunctionRule.Options.options

    let options =
      { DisallowedFunctionRule.Options.disallowed_function
      ; DisallowedFunctionRule.Options.suggested_function= Some suggested_function }
  end) in
  (module M : Rule.HASRULE)

let createNoJStringInterpolationRule () = (module NoJStringInterpolationRule: Rule.HASRULE)

let createNoReactComponentRule options =
  let open Yojson.Basic.Util in
  let component_name = options |> member "component" |> to_string in
  let module M = NoReactComponentRule.Make (struct
    type options = NoReactComponentRule.Options.options

    let options = {NoReactComponentRule.Options.component_name}
  end) in
  (module M : Rule.HASRULE)

let createDisallowModuleRule options =
  let open Yojson.Basic.Util in
  let disallowed_module = options |> member "disallowed_module" |> to_string in
  let suggested_module = options |> member "suggested_module" |> to_string in
  let module M = DisallowModuleRule.Make (struct
    type options = DisallowModuleRule.Options.options

    let options =
      { DisallowModuleRule.Options.disallowed_module
      ; DisallowModuleRule.Options.suggested_module= Some suggested_module }
  end) in
  (module M : Rule.HASRULE)

let parseConfig path =
  let json = Yojson.Basic.from_file path in
  let open Yojson.Basic.Util in
  let filter_rule json =
    let rule = json |> member "rule" |> to_string in
    match rule with
    | "DisallowOperator" ->
        let options = json |> member "options" in
        createDisallowOperatorRule options
    | "DisallowFunction" ->
        let options = json |> member "options" in
        createDisallowFunctionRule options
    | "NoJStringInterpolation" -> createNoJStringInterpolationRule ()
    | "NoReactComponent" ->
        let options = json |> member "options" in
        createNoReactComponentRule options
    | "DisallowModule" ->
        let options = json |> member "options" in
        createDisallowModuleRule options
    | _ -> raise RuleDoesNotExist
  in
  json |> member "rules" |> to_list |> List.map filter_rule
