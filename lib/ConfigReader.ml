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
    | _ -> raise RuleDoesNotExist
  in
  json |> member "rules" |> to_list |> List.map filter_rule