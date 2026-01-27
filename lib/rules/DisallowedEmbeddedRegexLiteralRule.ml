let rec mapSnd (lst : ('a * 'b) list) : 'b list = match lst with [] -> [] | h :: t -> snd h :: mapSnd t

let rec map (f : 'a -> 'b) (lst : 'a list) : 'b list = match lst with [] -> [] | h :: t -> f h :: map f t

let rec forEach (lst : 'a list) (f : 'a -> unit) : unit =
  match lst with [] -> () | h :: t -> f h ; forEach t f

let find_arg_loc args =
  let f (_label, expr) =
    match expr with
    | {Parsetree.pexp_desc= Parsetree.Pexp_extension ({txt= "re"}, _); Parsetree.pexp_loc= loc} -> Some loc
    | _ -> None
  in
  List.map f args |> List.find (fun x -> x <> None) |> Option.get

let containsRegex args =
  let f (_label, expr) =
    match expr with {Parsetree.pexp_desc= Parsetree.Pexp_extension ({txt= "re"}, _)} -> true | _ -> false
  in
  List.exists f args

let rec getFilesInPath path =
  let files = Sys.readdir path in
  let files_with_path = Array.map (fun file -> Filename.concat path file) files in
  let files_only = Array.to_list files_with_path in
  let subdirectories = List.filter Sys.is_directory files_only in
  let files_in_subdirectories = List.map getFilesInPath subdirectories |> List.concat in
  let files_in_current_directory =
    List.filter (fun file -> (not (Sys.is_directory file)) && Filename.extension file = ".res") files_only
  in
  files_in_current_directory @ files_in_subdirectories

let getFileSource path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel ; src

let mapOverFilesInPath path f =
  let files = getFilesInPath path in
  forEach files f

let intercalate sep lst =
  let rec loop acc = function [] -> acc | [x] -> acc ^ x | x :: xs -> loop (acc ^ x ^ sep) xs in
  loop "" lst

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done ;
    false
  with Exit -> true

let safeHead lst = match lst with [] -> None | h :: _ -> Some h

let getAllVariablesUsedInJsReTest src =
  let p = Res_parser.make ~mode:Res_parser.Default src "." in
  let variables = ref [] in
  let callback (pair : string * Warnings.loc) = variables := !variables @ [pair] in
  let lintFunc expr =
    match expr with
    | {Parsetree.pexp_desc= Parsetree.Pexp_apply {funct = {pexp_desc= Pexp_ident {txt= exprTxt; loc= expr_loc}}; args = args}}
      when contains (intercalate "." (Longident.flatten exprTxt)) "Js.Re.test_" -> (
      match safeHead args with
      | Some (_label, {Parsetree.pexp_desc= Parsetree.Pexp_ident {txt= lident}}) ->
          Rule.LintError (Longident.last lident, expr_loc)
      | _ -> LintOk )
    | _ -> LintOk
  in
  let iterator = Iterator.withExpression Ast_iterator.default_iterator lintFunc callback in
  let ast = Res_core.parse_implementation p in
  match p.diagnostics with
  | [] -> iterator.structure iterator ast ; !variables
  | diagnostics ->
      Res_diagnostics.print_report diagnostics src ;
      exit 1

let getAllVariablesUsedInJsReTests path =
  let files = getFilesInPath path in
  let f file =
    let src = getFileSource file in
    getAllVariablesUsedInJsReTest src
  in
  let variables = map f files in
  let all_variables = List.concat variables in
  all_variables

module Options = struct
  type options = {test_directory: string}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) (LinterOptions : Rule.LinterOptions) :
  Rule.HASRULE = struct
  include LinterOptions

  let description =
    "[Rescript] Do not use %re(...) directly, please store it in variable before use so that it can be \
     tested."

  let meta =
    { Rule.ruleName= "DisallowEmbeddedRegexLiteral"
    ; Rule.ruleIdentifier= "DisallowEmbeddedRegexLiteral" ^ "[%re]"
    ; Rule.ruleDescription= description }

  let disallowLiteralInFunctionsLinter =
    Rule.LintExpression
      (fun expr ->
        match expr with
        (* matches some_func(..., %re("regex")) *)
        | {Parsetree.pexp_desc= Pexp_apply {args = args; _}} when containsRegex args ->
            (* Rescript_parser.Printast.expression 0 Format.err_formatter expr ; *)
            let inner_loc = find_arg_loc args in
            Rule.LintError (meta.ruleDescription, inner_loc)
        | _ -> Rule.LintOk )

  let requireTestingLinter vars =
    Rule.LintStructureItem
      (fun expr ->
        match expr with
        (* matches let a = %re("regex")*)
        | { Parsetree.pstr_desc=
              Pstr_value
                (_rec_flag, [{pvb_pat= {ppat_desc= Ppat_var {txt= var_name; loc= var_loc}}; pvb_expr= expr}])
          }
          when (not (List.exists (fun (msg, _loc) -> msg = var_name) vars)) && containsRegex [("", expr)] ->
            Rule.LintError
              ("This regex variable is not tested using Js.Re.test_ in any known testing file", var_loc)
        | _ -> Rule.LintOk )

  let linters =
    [ disallowLiteralInFunctionsLinter
    ; (let vars = getAllVariablesUsedInJsReTests OPT.options.test_directory in
       requireTestingLinter vars ) ]
end
