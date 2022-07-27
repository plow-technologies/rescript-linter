open Rescript_parser

module DisallowStringOfIntRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
end)

module DisallowIntOfStringOptRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "intOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
end)

module DisallowFloatOfStringOptRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "floatOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Float.fromString" }
end)

module DisallowTriangleOperatorRule = DisallowedOperatorRule.Make (struct
  type options = DisallowedOperatorRule.Options.options

  let options =
    { DisallowedOperatorRule.Options.disallowed_operator= "|>"
    ; DisallowedOperatorRule.Options.suggested_operator= Some "->" }
end)

let rules =
  [ (module DisallowStringOfIntRule : Rule.HASRULE)
  ; (module DisallowIntOfStringOptRule : Rule.HASRULE)
  ; (module DisallowFloatOfStringOptRule : Rule.HASRULE)
  ; (module DisallowTriangleOperatorRule : Rule.HASRULE)
  ; (module NoJStringInterpolation.Rule : Rule.HASRULE)
  ; (module JSXRule.Rule : Rule.HASRULE) ]

let processFile path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel ; src

let lint rules structure =
  let errors = ref [] in
  let callback (pair : string * Location.t) = errors := !errors @ [pair] in
  let iterator = Iterator.makeIterator rules callback in
  iterator.structure iterator structure ;
  !errors

let run path =
  let src = processFile path in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  let p = Res_parser.make ~mode:Res_parser.Default src path in
  let structure = Res_core.parseImplementation p in
  match p.diagnostics with
  | [] -> (
      let errors = lint rules structure in
      match errors with
      | [] -> print_endline "All good"
      | xs ->
          let f (msg, loc) = Printer.printError src msg loc in
          List.iter f xs )
  | diagnostics ->
      (* parser contains problems *)
      Res_diagnostics.printReport diagnostics src
