open Rescript_parser

let processFile path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel ; src

let filename = "./test/foo.res"

let src = processFile filename

let p =
  (* if you want to target the printer use: let mode = Res_parser.Default
     in*)
  Res_parser.make ~mode:Res_parser.Default src filename

let structure = Res_core.parseImplementation p

let signature = Res_core.parseSpecification p

module DisallowStringOfIntRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
    ; DisallowedFunctionRule.Options.suggested_function=
        Some "Belt.Int.fromString" }
end)

module DisallowIntOfStringOptRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "intOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function=
        Some "Belt.Int.fromString" }
end)

module DisallowFloatOfStringOptRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "floatOfStringOpt"
    ; DisallowedFunctionRule.Options.suggested_function=
        Some "Belt.Float.fromString" }
end)

let rules =
  [ (module DisallowStringOfIntRule : Rule.HASRULE)
  ; (module DisallowIntOfStringOptRule : Rule.HASRULE)
  ; (module DisallowFloatOfStringOptRule : Rule.HASRULE)
  ; (module NoJStringInterpolation.Rule : Rule.HASRULE) ]

let run =
  match p.diagnostics with
  | [] ->
      let iterator = Iterator.makeIterator p rules in
      iterator.structure iterator structure
  | diagnostics ->
      (* parser contains problems *)
      Res_diagnostics.printReport diagnostics src
